package easyrider.business.core

import akka.actor._
import akka.event.LoggingReceive
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import easyrider.Events.{Subscribe, GetSnapshot, GetSnapshotResponse}
import easyrider.Implicits._
import easyrider.Infrastructure._
import easyrider._
import easyrider.business.core.ApplicationManager.RestoredConfiguration

import scala.concurrent.duration._

class ApplicationManager(val eventBus: ActorRef, infrastructure: ActorRef) extends Actor with Stash with ActorLogging with EventPublisher {
  import easyrider.Applications._
  private var applications = Map[ApplicationId, Application]()
  private var stages = Map[StageId, Stage]()
  private var containers = Map[ContainerId, ContainerConfiguration]()

  // TODO: make sure there's a proper failure after this timeout
  eventBus ! Subscribe(CommandDetails(), "containerStateUpdates", classOf[ContainerStateChangedEvent], EventKey())
  implicit val timeout = Timeout(30 seconds)
  implicit val dispatcher = context.system.dispatcher
  val appsFuture = eventBus ? GetSnapshot(CommandDetails(), SnapshotEntryType(classOf[Application]))
  val stagesFuture = eventBus ? GetSnapshot(CommandDetails(), SnapshotEntryType(classOf[Stage]))
  val containersFuture = eventBus ? GetSnapshot(CommandDetails(), SnapshotEntryType(classOf[ContainerConfiguration]))
  private val restoredConfiguration = for {
    apps <- appsFuture
    stages <- stagesFuture
    containers <- containersFuture
  } yield RestoredConfiguration(
      applications = apps.asInstanceOf[GetSnapshotResponse[Application]].snapshot.entries.map { case (_, app) => app.id -> app },
      stages = stages.asInstanceOf[GetSnapshotResponse[Stage]].snapshot.entries.map { case (_, stage) => stage.id -> stage },
      containers = containers.asInstanceOf[GetSnapshotResponse[ContainerConfiguration]].snapshot.entries.map { case (_, container) => container.id -> container })
  restoredConfiguration pipeTo self

  def initializing: Receive = {
    case restored: RestoredConfiguration =>
      applications = restored.applications
      stages = restored.stages
      containers = restored.containers
      unstashAll()
      context.become(running)
    case other => stash()
  }

  def running = LoggingReceive {
    case command @ CreateApplication(commandDetails, application) => application match {
      case ExistingApplication(_) => sender ! command.failure(s"Application ${application.id.id} already exists")
      case _ =>
        applications += (application.id -> application)
        val snapshotUpdate = SnapshotUpdateDetails(SnapshotEntryType(classOf[Application]), application.id.eventKey, Some(application))
        publishEvent(ApplicationUpdatedEvent(EventDetails(EventId.generate(), EventKey(application.id.id), Seq(commandDetails.commandId)), commandDetails.commandId, snapshotUpdate))
    }
    case command @ RemoveApplication(commandDetails, applicationId) => applicationId match {
      case NonExistingApplication(_) => sender ! command.failure(s"Application ${applicationId.id} does not exist")
      case ApplicationWithStages(_) => sender ! command.failure(s"Remove all stages from ${applicationId.id} first")
      case ExistingApplication(application) =>
        applications -= applicationId
        val snapshotUpdate = SnapshotUpdateDetails[Application](SnapshotEntryType(classOf[Application]), application.id.eventKey, None)
        publishEvent(ApplicationUpdatedEvent(EventDetails(EventId.generate(), application.id.eventKey, Seq(commandDetails.commandId), removal = true), commandDetails.commandId, snapshotUpdate))
    }
    case command @ UpdateApplication(commandDetails, application) => application match {
      case NonExistingApplication(_) => sender ! command.failure(s"Application ${application.id} does not exist")
      case _ =>
        applications += (application.id -> application)
        val snapshotUpdate = SnapshotUpdateDetails(SnapshotEntryType(classOf[Application]), application.id.eventKey, Some(application))
        publishEvent(ApplicationUpdatedEvent(EventDetails(EventId.generate(), application.id.eventKey, Seq(commandDetails.commandId)), commandDetails.commandId, snapshotUpdate))
        updateEffectiveConfigurationForContainersThat(_.id.stageId.applicationId == application.id, commandDetails.commandId)
    }
    case command @ CreateStage(commandDetails, stage) => stage match {
      case NonExistingApplication(_) => sender ! command.failure(s"Application ${stage.id.applicationId.id} does not exist")
      case ExistingStage(_) => sender ! command.failure(s"Stage ${stage.id.id} for application ${stage.id.applicationId.id} is already defined")
      case _ =>
        stages += (stage.id -> stage)
        val snapshotUpdate = SnapshotUpdateDetails(SnapshotEntryType(classOf[Stage]), stage.id.eventKey, Some(stage))
        publishEvent(StageUpdatedEvent(EventDetails(EventId.generate(), stage.id.eventKey, Seq(commandDetails.commandId)), stage, snapshotUpdate, commandDetails.commandId))
    }
    case command @ RemoveStage(commandDetails, stageId) => stageId match {
      case NonExistingStage(_) => sender ! command.failure(s"Stage ${stageId.id} of application ${stageId.applicationId.id} does not exist")
      case ExistingStage(stage) =>
        stages -= stageId
        val snapshotUpdate = SnapshotUpdateDetails[Stage](SnapshotEntryType(classOf[Stage]), stage.id.eventKey, None)
        publishEvent(StageUpdatedEvent(EventDetails(EventId.generate(), stage.id.eventKey, Seq(commandDetails.commandId), removal = true), stage, snapshotUpdate, commandDetails.commandId))
    }
    case command @ UpdateStage(commandDetails, stage) => stage.id match {
      case NonExistingStage(_) => sender ! command.failure(s"Stage ${stage.id.id} of application ${stage.id.applicationId.id}")
      case ExistingStage(_) =>
        stages += (stage.id -> stage)
        val snapshotUpdate = SnapshotUpdateDetails(SnapshotEntryType(classOf[Stage]), stage.id.eventKey, Some(stage))
        publishEvent(StageUpdatedEvent(EventDetails(EventId.generate(), stage.id.eventKey, Seq(commandDetails.commandId)), stage, snapshotUpdate, commandDetails.commandId))
        updateEffectiveConfigurationForContainersThat(_.id.stageId == stage.id, commandDetails.commandId)
    }
    case command @ CreateContainerConfiguration(commandDetails, container) => container match {
      case ExistingContainer(_) => sender ! command.failure(s"Container ${container.id.id} in application ${container.id.stageId.applicationId.id} stage ${container.id.stageId.id} already exists")
      case NonExistingStage(_) => sender ! command.failure(s"Stage ${container.id.stageId.id} of application ${container.id.stageId.applicationId.id} does not exist")
      case _ =>
        val snapshotUpdate = SnapshotUpdateDetails(SnapshotEntryType(classOf[ContainerConfiguration]), container.id.eventKey, Some(container))
        // TODO: can this be correlated with original command? what if it fails?
        infrastructure.forward(CreateContainer(CommandDetails(), container.nodeId, container.id))
        containers += (container.id -> container)
        publishEvent(ContainerConfigurationUpdatedEvent(EventDetails(EventId.generate(), container.id.eventKey, Seq(commandDetails.commandId)), container, snapshotUpdate = snapshotUpdate, executionOf = commandDetails.commandId))
        eventBus ! EffectiveConfigurationChanged(EventDetails(EventId.generate(), container.id.eventKey, Seq(commandDetails.commandId)), container.id, getEffectiveConfiguration(container.id).get)
    }
    case command @ UpdateContainerConfiguration(commandDetails, container) => container match {
      case NonExistingContainer(_) => sender ! command.failure(s"Container ${container.id.id} does not exist in application ${container.id.stageId.applicationId.id} stage ${container.id.stageId.id}")
      case _ =>
        containers += (container.id -> container)
        val snapshotUpdate = SnapshotUpdateDetails(SnapshotEntryType(classOf[ContainerConfiguration]), container.id.eventKey, Some(container))
        publishEvent(ContainerConfigurationUpdatedEvent(EventDetails(EventId.generate(), container.id.eventKey, Seq(commandDetails.commandId)), container, snapshotUpdate = snapshotUpdate, executionOf = commandDetails.commandId))
        eventBus ! EffectiveConfigurationChanged(EventDetails(EventId.generate(), container.id.eventKey, Seq(commandDetails.commandId)), container.id, getEffectiveConfiguration(container.id).get)
    }
    case command: ContainerCommand =>
      containers.get(command.containerId) match {
        // TODO: use the applications' container-type
        case Some(container) => infrastructure.forward(AddressedContainerCommand("builtin", container.nodeId, command))
        case None => sender ! command.failure(s"Container ${command.containerId.containerName} does not exist")
      }
    case ContainerStateChangedEvent(eventDetails, containerId, state, _) => state match {
      case ContainerRemoved =>
        containers.get(containerId) match {
          case Some(container) =>
            eventBus ! ContainerConfigurationRemoved(EventDetails(EventId.generate(), containerId.eventKey, Seq(eventDetails.eventId), removal = true), SnapshotUpdateDetails(SnapshotEntryType(classOf[ContainerConfiguration]), containerId.eventKey, None))
            containers -= containerId
          case None => // do nothing
        }
      case _ =>
    }
  }

  def updateEffectiveConfigurationForContainersThat(predicate: (ContainerConfiguration) => Boolean, cause: Cause) {
    containers.values.filter(predicate).foreach { container =>
      eventBus ! EffectiveConfigurationChanged(EventDetails(EventId.generate(), container.id.eventKey, Seq(cause)), container.id, getEffectiveConfiguration(container.id).get)
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

  private case class RestoredConfiguration(applications: Map[ApplicationId, Application], stages: Map[StageId, Stage], containers: Map[ContainerId, ContainerConfiguration])
}
