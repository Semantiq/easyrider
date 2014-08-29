package easyrider.business.core

import akka.actor.{Actor, ActorRef, Props}
import easyrider.Infrastructure.{AddressedContainerCommand, ContainerCommand}
import easyrider.{EventDetails, EventId, EventKey}

class ApplicationManager(eventBus: ActorRef, infrastructure: ActorRef) extends Actor {
  import easyrider.Applications._
  private var applications = Map[ApplicationId, Application]()
  private var stages = Map[StageId, Stage]()
  private var containers = Map[ContainerId, ContainerConfiguration]()

  override def receive: Receive = {
    case command @ CreateApplication(commandId, application) => application match {
      case ExistingApplication(_) => sender ! command.failure(s"Application ${application.id.id} already exists")
      case _ =>
        applications += (application.id -> application)
        eventBus ! ApplicationUpdatedEvent(EventDetails(EventId.generate(), EventKey(application.id.id), Seq(commandId)), application)
    }
    case command @ RemoveApplication(commandId, applicationId) => applicationId match {
      case NonExistingApplication(_) => sender ! command.failure(s"Application ${applicationId.id} does not exist")
      case ApplicationWithStages(_) => sender ! command.failure(s"Remove all stages from ${applicationId.id} first")
      case ExistingApplication(application) =>
        applications -= applicationId
        eventBus ! ApplicationUpdatedEvent(EventDetails(EventId.generate(), application.id.eventKey, Seq(commandId), removal = true), application)
    }
    case command @ UpdateApplication(commandId, application) => application match {
      case NonExistingApplication(_) => sender ! command.failure(s"Application ${application.id} does not exist")
      case _ =>
        applications += (application.id -> application)
        eventBus ! ApplicationUpdatedEvent(EventDetails(EventId.generate(), application.id.eventKey, Seq(commandId)), application)
    }
    case command @ CreateStage(commandId, stage) => stage match {
      case NonExistingApplication(_) => sender ! command.failure(s"Application ${stage.id.applicationId.id} does not exist")
      case ExistingStage(_) => sender ! command.failure(s"Stage ${stage.id.id} for application ${stage.id.applicationId.id} is already defined")
      case _ =>
        stages += (stage.id -> stage)
        eventBus ! StageUpdatedEvent(EventDetails(EventId.generate(), stage.id.eventKey, Seq(commandId)), stage)
    }
    case command @ RemoveStage(commandId, stageId) => stageId match {
      case NonExistingStage(_) => sender ! command.failure(s"Stage ${stageId.id} of application ${stageId.applicationId.id} does not exist")
      case ExistingStage(stage) =>
        stages -= stageId
        eventBus ! StageUpdatedEvent(EventDetails(EventId.generate(), stage.id.eventKey, Seq(commandId), removal = true), stage)
    }
    case command @ CreateContainerConfiguration(commandId, container) => container match {
      case ExistingContainer(_) => sender ! command.failure(s"Container ${container.id.id} in application ${container.id.stageId.applicationId.id} stage ${container.id.stageId.id} already exists")
      case NonExistingStage(_) => sender ! command.failure(s"Stage ${container.id.stageId.id} of application ${container.id.stageId.applicationId.id} does not exist")
      case _ =>
        containers += (container.id -> container)
        eventBus ! ContainerConfigurationUpdatedEvent(EventDetails(EventId.generate(), container.id.eventKey, Seq(commandId)), container)
    }
    case command @ UpdateContainerConfiguration(commandId, container) => container match {
      case NonExistingContainer(_) => sender ! command.failure(s"Container ${container.id.id} does not exist in application ${container.id.stageId.applicationId.id} stage ${container.id.stageId.id}")
      case _ =>
        containers += (container.id -> container)
        eventBus ! ContainerConfigurationUpdatedEvent(EventDetails(EventId.generate(), container.id.eventKey, Seq(commandId)), container)
    }
    case command: ContainerCommand =>
      containers.get(command.containerId) match {
        case Some(container) => infrastructure.forward(AddressedContainerCommand(container.nodeId, command))
        case None => sender ! command.failure(s"Container ${command.containerId.containerName} does not exist")
      }
  }

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
  def apply(eventBus: ActorRef, infrastructure: ActorRef) = Props(classOf[ApplicationManager], eventBus, infrastructure)
}
