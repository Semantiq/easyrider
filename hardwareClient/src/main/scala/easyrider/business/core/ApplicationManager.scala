package easyrider.business.core

import akka.actor.{ActorRef, Actor, Props}
import easyrider.{EventKey, EventId, EventDetails}

class ApplicationManager(eventBus: ActorRef) extends Actor {
  import easyrider.Applications._
  private var applications = Map[ApplicationId, Application]()

  override def receive: Receive = {
    case command @ CreateApplication(commandId, application) =>
      if (applications.contains(application.id)) {
        sender ! command.failure(s"Application ${application.id.id} already exists")
      } else {
        applications += (application.id -> application)
        eventBus ! ApplicationUpdatedEvent(EventDetails(EventId.generate(), EventKey(application.id.id), Seq(commandId)), application)
      }
    case command @ RemoveApplication(commandId, applicationId) =>
      if (!applications.contains(applicationId)) {
        sender ! command.failure(s"Application ${applicationId.id} does not exist")
      } else {
        val application = applications(applicationId)
        applications -= applicationId
        eventBus ! ApplicationUpdatedEvent(EventDetails(EventId.generate(), EventKey(applicationId.id), Seq(commandId), removal = true), application)
      }
  }
}

object ApplicationManager {
  def apply(eventBus: ActorRef) = Props(classOf[ApplicationManager], eventBus)
}