package eu.semantiq.easyrider

import akka.actor._
import java.io.File

class LeadingActor extends Actor with Stash {
  import LeadingActor._

  context.actorOf(Props(classOf[ConfigurationManager], self, configFileLocation), "configuration-manager")

  def configured(configuration: Seq[Application]): Receive = {
    case Start =>
      val appSupervisors = configuration map {
        app => context.actorOf(Props(classOf[AppSupervisor], app), s"${app.name}-supervisor")
      }
      appSupervisors foreach { _ ! AppSupervisor.Start }
      context.become(running(configuration, appSupervisors))
  }

  def running(configuration: Seq[Application], appSupervisors: Seq[ActorRef]): Receive = {
    case Stop => context.stop(self)
  }

  def receive: Receive = {
    case ConfigurationManager.Reconfigured(configuration) =>
      unstashAll()
      context.become(configured(configuration))
    case _ => stash()
  }

  private def configFileLocation: File = new File(System.getProperty("configuration", "configuration.json"))
}

object LeadingActor {
  object Start
  object Stop
}
