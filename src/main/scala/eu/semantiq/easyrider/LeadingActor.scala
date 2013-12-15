package eu.semantiq.easyrider

import akka.actor._
import java.io.File
import scala.concurrent.duration._
import eu.semantiq.easyrider.AppSupervisor.ConfigurationUpdated

class LeadingActor extends Actor with Stash {
  import LeadingActor._

  context.actorOf(Props(classOf[ConfigurationManager], self, configFileLocation, 10.seconds), "configuration-manager")

  def configured(configuration: Seq[Application]): Receive = {
    case Start =>
      configuration foreach {
        app => context.actorOf(Props(classOf[AppSupervisor]), app.name)
      }
      configuration.foreach(app => context.child(app.name).get ! ConfigurationUpdated(app))
      context.become(running(configuration))
  }

  def running(configuration: Seq[Application]): Receive = {
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
