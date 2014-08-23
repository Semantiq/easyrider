package easyrider.business.core

import akka.actor.{ActorRef, Actor, Props}
import easyrider.{EventKey, EventId, EventDetails}

class ApplicationManager(eventBus: ActorRef) extends Actor {
  import easyrider.Applications._
  private var applications = Map[ApplicationId, Application]()
  private var stages = Map[StageId, Stage]()

  override def receive: Receive = {
    case command @ CreateApplication(commandId, application) =>
      application match {
        case ExistingApplication(_) => sender ! command.failure(s"Application ${application.id.id} already exists")
        case _ =>
          applications += (application.id -> application)
          eventBus ! ApplicationUpdatedEvent(EventDetails(EventId.generate(), EventKey(application.id.id), Seq(commandId)), application)
      }
    case command @ RemoveApplication(commandId, applicationId) =>
      applicationId match {
        case NonExistingApplication(_) => sender ! command.failure(s"Application ${applicationId.id} does not exist")
        case ApplicationWithStages(_) => sender ! command.failure(s"Remove all stages from ${applicationId.id} first")
        case ExistingApplication(application) =>
          applications -= applicationId
          eventBus ! ApplicationUpdatedEvent(EventDetails(EventId.generate(), EventKey(applicationId.id), Seq(commandId), removal = true), application)
      }
    case command @ UpdateApplication(commandId, application) =>
      application match {
        case NonExistingApplication(_) => sender ! command.failure(s"Application ${application.id} does not exist")
        case _ =>
          applications += (application.id -> application)
          eventBus ! ApplicationUpdatedEvent(EventDetails(EventId.generate(), EventKey(application.id.id), Seq(commandId)), application)
      }
    case command @ CreateStage(commandId, stage) =>
      stage match {
        case NonExistingApplication(_) => sender ! command.failure(s"Application ${stage.id.applicationId.id} does not exist")
        case ExistingStage(_) => sender ! command.failure(s"Stage ${stage.id.id} for application ${stage.id.applicationId.id} is already defined")
        case _ =>
          stages += (stage.id -> stage)
          eventBus ! StageUpdatedEvent(EventDetails(EventId.generate(), EventKey(stage.id.applicationId.id, stage.id.id), Seq(commandId)), stage)
      }
    case command @ RemoveStage(commandId, stageId) =>
      stageId match {
        case NonExistingStage(_) => command.failure(s"Stage ${stageId.id} of application ${stageId.applicationId.id} does not exist")
        case ExistingStage(stage) =>
          stages -= stageId
          eventBus ! StageUpdatedEvent(EventDetails(EventId.generate(), EventKey(stage.id.applicationId.id, stage.id.id), Seq(commandId), removal = true), stage)
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
    def unapply(stageId: StageId) = if (stages.contains(stageId)) None else Some(stageId)
  }
}

object ApplicationManager {
  def apply(eventBus: ActorRef) = Props(classOf[ApplicationManager], eventBus)
}