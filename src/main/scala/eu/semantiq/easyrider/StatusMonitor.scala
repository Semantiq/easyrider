package eu.semantiq.easyrider

import akka.actor.{ActorLogging, Actor}
import eu.semantiq.easyrider.AppSupervisor.AppLifecycleEvent

class StatusMonitor extends Actor with ActorLogging {
  context.system.eventStream.subscribe(self, classOf[AppLifecycleEvent])

  def receive: Actor.Receive = {
    case event: AppLifecycleEvent => log.info("lifecycle event: " + event)

  }
}

object StatusMonitor {
  object GetStatus
  case class Status(apps: Map[String, AppStatus])
  case class AppStatus()
}
