package eu.semantiq.easyrider

import akka.actor.{ActorLogging, Actor}
import eu.semantiq.easyrider.supervisor.AppSupervisor
import AppSupervisor.AppLifecycleEvent
import eu.semantiq.easyrider.builder.AppBuilder.BuildEvent

class StatusMonitor extends Actor with ActorLogging {
  import StatusMonitor._
  context.system.eventStream.subscribe(self, classOf[AppLifecycleEvent])
  context.system.eventStream.subscribe(self, classOf[BuildEvent])

  var status = Map[String, AppStatus]()

  def receive: Actor.Receive = {
    case event: AppLifecycleEvent =>
      log.info("lifecycle event: " + event)
      val newAppStatus = status.getOrElse(event.app, AppStatus()).copy(process = Some(event.toString))
      updateStatus(event.app, newAppStatus)
    case event: BuildEvent =>
      log.info("build event: " + event)
      val newAppStatus = status.getOrElse(event.app, AppStatus()).copy(build = Some(event.toString))
      updateStatus(event.app, newAppStatus)
    case GetStatus => sender ! Status(status)
  }

  private def updateStatus(app: String, newAppStatus: StatusMonitor.AppStatus) {
    status = status.updated(app, newAppStatus)
    context.system.eventStream.publish(Status(status))
  }
}

object StatusMonitor {
  object GetStatus
  case class AppStatus(process: Option[String] = None, build: Option[String] = None)
  case class Status(apps: Map[String, AppStatus])
}
