package eu.semantiq.easyrider

import akka.actor.{ActorLogging, Actor}
import eu.semantiq.easyrider.AppSupervisor.AppLifecycleEvent
import eu.semantiq.easyrider.StatusMonitor.{Status, GetStatus}

class StatusMonitor extends Actor with ActorLogging {
  context.system.eventStream.subscribe(self, classOf[AppLifecycleEvent])

  var status = Map[String, String]()

  def receive: Actor.Receive = {
    case event: AppLifecycleEvent =>
      log.info("lifecycle event: " + event)
      status = status.updated(event.app, event.toString)
      context.system.eventStream.publish(Status(status))
    case GetStatus => sender ! Status(status)
  }
}

object StatusMonitor {
  object GetStatus
  case class Status(apps: Map[String, String])
}
