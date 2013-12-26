package eu.semantiq.easyrider

import akka.actor._
import java.io.File
import scala.concurrent.duration._
import eu.semantiq.easyrider.AppSupervisor.ConfigurationUpdated

class LeadingActor extends Actor with Stash {
  import LeadingActor._

  context.actorOf(Props(classOf[ConfigurationManager], self, configFileLocation, 10.seconds), "configuration-manager")
  private val statusMonitor = context.actorOf(Props[StatusMonitor], "status-monitor")
  private val dispatcher = context.actorOf(HttpDispatcher(statusMonitor), "http-dispatcher")

  def configured(configuration: Seq[Application]): Receive = {
    case Start =>
      configuration foreach {
        app => context.actorOf(AppSupervisor(new File(workingDirectory, app.name)), app.name)
      }
      configuration.foreach(app => context.child(app.name).get ! ConfigurationUpdated(app))
      dispatcher ! HttpDispatcher.NewConfiguration(8080)
      context.become(running(configuration))
    //case ConfigurationManager.Reconfigured(configuration) =>
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
  private def workingDirectory: File = new File(System.getProperty("working.directory", "working"))
}

object LeadingActor {
  object Start
  object Stop
}
