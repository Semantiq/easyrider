package easyrider.business.core

import akka.actor.{Stash, Actor, ActorRef, Props}
import akka.pattern.ask
import akka.pattern.pipe
import akka.util.Timeout
import easyrider.Configuration.{EffectiveConfiguration, DeployConfiguration}
import easyrider.Events.{GetSnapshot, GetSnapshotResponse}
import easyrider.Implicits._
import easyrider.Infrastructure.{AddressedContainerCommand, ContainerCommand, CreateContainer}
import easyrider._
import easyrider.business.core.ApplicationManager.RestoredConfiguration

import scala.concurrent.duration._

class ApplicationManager(eventBus: ActorRef, infrastructure: ActorRef) extends Actor with Stash {
  import easyrider.Applications._
  private var applications = Map[ApplicationId, Application]()
  private var stages = Map[StageId, Stage]()
  private var containers = Map[ContainerId, ContainerConfiguration]()

  implicit val timeout = Timeout(3 seconds)
  implicit val dispatcher = context.system.dispatcher
  val appsFuture = eventBus ? GetSnapshot(QueryId.generate(), classOf[ApplicationUpdatedEvent])
  val stagesFuture = eventBus ? GetSnapshot(QueryId.generate(), classOf[StageUpdatedEvent])
  val containersFuture = eventBus ? GetSnapshot(QueryId.generate(), classOf[ContainerConfigurationUpdatedEvent])
  private val restoredConfiguration = for {
    apps <- appsFuture
    stages <- stagesFuture
    containers <- containersFuture
  } yield RestoredConfiguration(
      applications = apps.asInstanceOf[GetSnapshotResponse[ApplicationUpdatedEvent]].snapshot.map { case ApplicationUpdatedEvent(_, app) => app },
      stages = stages.asInstanceOf[GetSnapshotResponse[StageUpdatedEvent]].snapshot.map { case StageUpdatedEvent(_, stage) => stage },
      containers = containers.asInstanceOf[GetSnapshotResponse[ContainerConfigurationUpdatedEvent]].snapshot.map { case ContainerConfigurationUpdatedEvent(_, container) => container })
  restoredConfiguration pipeTo self

  def initializing: Receive = {
    case restored: RestoredConfiguration =>
      def unpack[T, K](map: Map[T, Seq[K]]) = map.map { case (key, Seq(value)) => (key, value) }
      applications = unpack(restored.applications.groupBy(_.id))
      stages = unpack(restored.stages.groupBy(_.id))
      containers = unpack(restored.containers.groupBy(_.id))
      unstashAll()
      context.become(running)
    case other => stash()
  }

  def running: Receive = {
    case command @ CreateApplication(commandDetails, application) => application match {
      case ExistingApplication(_) => sender ! command.failure(s"Application ${application.id.id} already exists")
      case _ =>
        applications += (application.id -> application)
        eventBus ! ApplicationUpdatedEvent(EventDetails(EventId.generate(), EventKey(application.id.id), Seq(commandDetails.commandId)), application)
    }
    case command @ RemoveApplication(commandDetails, applicationId) => applicationId match {
      case NonExistingApplication(_) => sender ! command.failure(s"Application ${applicationId.id} does not exist")
      case ApplicationWithStages(_) => sender ! command.failure(s"Remove all stages from ${applicationId.id} first")
      case ExistingApplication(application) =>
        applications -= applicationId
        eventBus ! ApplicationUpdatedEvent(EventDetails(EventId.generate(), application.id.eventKey, Seq(commandDetails.commandId), removal = true), application)
    }
    case command @ UpdateApplication(commandDetails, application) => application match {
      case NonExistingApplication(_) => sender ! command.failure(s"Application ${application.id} does not exist")
      case _ =>
        applications += (application.id -> application)
        eventBus ! ApplicationUpdatedEvent(EventDetails(EventId.generate(), application.id.eventKey, Seq(commandDetails.commandId)), application)
    }
    case command @ CreateStage(commandDetails, stage) => stage match {
      case NonExistingApplication(_) => sender ! command.failure(s"Application ${stage.id.applicationId.id} does not exist")
      case ExistingStage(_) => sender ! command.failure(s"Stage ${stage.id.id} for application ${stage.id.applicationId.id} is already defined")
      case _ =>
        stages += (stage.id -> stage)
        eventBus ! StageUpdatedEvent(EventDetails(EventId.generate(), stage.id.eventKey, Seq(commandDetails.commandId)), stage)
    }
    case command @ RemoveStage(commandDetails, stageId) => stageId match {
      case NonExistingStage(_) => sender ! command.failure(s"Stage ${stageId.id} of application ${stageId.applicationId.id} does not exist")
      case ExistingStage(stage) =>
        stages -= stageId
        eventBus ! StageUpdatedEvent(EventDetails(EventId.generate(), stage.id.eventKey, Seq(commandDetails.commandId), removal = true), stage)
    }
    case command @ CreateContainerConfiguration(commandDetails, container) => container match {
      case ExistingContainer(_) => sender ! command.failure(s"Container ${container.id.id} in application ${container.id.stageId.applicationId.id} stage ${container.id.stageId.id} already exists")
      case NonExistingStage(_) => sender ! command.failure(s"Stage ${container.id.stageId.id} of application ${container.id.stageId.applicationId.id} does not exist")
      case _ =>
        // TODO: can this be correlated with original command? what if it fails?
        infrastructure.forward(CreateContainer(CommandDetails(CommandId.generate(), TraceMode()), container.nodeId, container.id))
        containers += (container.id -> container)
        infrastructure ! AddressedContainerCommand(container.nodeId, DeployConfiguration(CommandDetails(CommandId.generate(), TraceMode()), container.id, getEffectiveConfiguration(container.id).get))
        eventBus ! ContainerConfigurationUpdatedEvent(EventDetails(EventId.generate(), container.id.eventKey, Seq(commandDetails.commandId)), container)
    }
    case command @ UpdateContainerConfiguration(commandDetails, container) => container match {
      case NonExistingContainer(_) => sender ! command.failure(s"Container ${container.id.id} does not exist in application ${container.id.stageId.applicationId.id} stage ${container.id.stageId.id}")
      case _ =>
        containers += (container.id -> container)
        eventBus ! ContainerConfigurationUpdatedEvent(EventDetails(EventId.generate(), container.id.eventKey, Seq(commandDetails.commandId)), container)
    }
    case command: ContainerCommand =>
      containers.get(command.containerId) match {
        case Some(container) => infrastructure.forward(AddressedContainerCommand(container.nodeId, command))
        case None => sender ! command.failure(s"Container ${command.containerId.containerName} does not exist")
      }
  }

  def getEffectiveConfiguration(containerId: ContainerId): Option[EffectiveConfiguration] = {
    def resolveProperties(properties: Seq[Property]) = properties.map(p => p.name -> p.value).toMap
    for (
      container <- containers.get(containerId);
      stage <- stages.get(containerId.stageId);
      app <- applications.get(containerId.stageId.applicationId)
    ) yield EffectiveConfiguration(resolveProperties(app.properties) ++ resolveProperties(stage.properties) ++ resolveProperties(container.properties))
  }

  override def receive = initializing

  object ExistingApplication {
    def unapply(applicationId: ApplicationId): Option[Application] = applications.get(applicationId)
    def unapply(application: Application): Option[Application] = unapply(application.id)
    def unapply(stage: Stage): Option[Application] = unapply(stage.id.applicationId)
  }
  object NonExistingApplication {
    def unapply(applicationId: ApplicationId): Option[ApplicationId] = if (applications.contains(applicationId)) None else Some(applicationId)
    def unapply(application: Application): Option[ApplicationId] = unapply(application.id)
    def unapply(stage: Stage): Option[ApplicationId] = unapply(stage.id.applicationId)
  }
  object ApplicationWithStages {
    def unapply(applicationId: ApplicationId) = {
      val appStages = stages.filter(s => s._1.applicationId == applicationId).map(_._2)
      if (appStages.isEmpty) None else Some(appStages)
    }
  }
  object ExistingStage {
    def unapply(stageId: StageId): Option[Stage] = stages.get(stageId)
    def unapply(stage: Stage): Option[Stage] = unapply(stage.id)
  }
  object NonExistingStage {
    def unapply(stageId: StageId): Option[StageId] = if (stages.contains(stageId)) None else Some(stageId)
    def unapply(container: ContainerConfiguration): Option[StageId] = unapply(container.id.stageId)
  }
  object ExistingContainer {
    def unapply(container: ContainerConfiguration): Option[ContainerConfiguration] = containers.get(container.id)
  }
  object NonExistingContainer {
    def unapply(container: ContainerConfiguration) = if (containers.contains(container.id)) None else Some(container.id)
  }
}

object ApplicationManager {
  import easyrider.Applications._

  def apply(eventBus: ActorRef, infrastructure: ActorRef) = Props(classOf[ApplicationManager], eventBus, infrastructure)

  private case class RestoredConfiguration(applications: Seq[Application], stages: Seq[Stage], containers: Seq[ContainerConfiguration])
}
