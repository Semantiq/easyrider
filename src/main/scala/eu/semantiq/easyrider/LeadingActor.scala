package eu.semantiq.easyrider

import akka.actor._
import java.io.File
import scala.concurrent.duration._
import eu.semantiq.easyrider.supervisor.AppSupervisor
import AppSupervisor.ConfigurationUpdated

class LeadingActor extends Actor with Stash {
  import LeadingActor._

  context.actorOf(Props(classOf[ConfigurationManager], self, configFileLocation, 10.seconds), "configuration-manager")
  private val statusMonitor = context.actorOf(Props[StatusMonitor], "status-monitor")
  private val dispatcher = context.actorOf(HttpDispatcher(statusMonitor), "http-dispatcher")

  def configured(configuration: Seq[Application]): Receive = {
    case Start =>
      configuration foreach createAppSupervisor
      dispatcher ! HttpDispatcher.NewConfiguration(8080)
      context.become(running(configuration))
    case ConfigurationManager.Reconfigured(newConfiguration) =>
      val newApps = newConfiguration.filterNot(app => configuration.exists(_.name == app.name))
      val removedApps = configuration.filterNot(app => newConfiguration.exists(_.name == app.name))
      val modifiedApps = newConfiguration.filter { app =>
        val oldApp = configuration.find(_.name == app.name)
        oldApp.isDefined && oldApp.get != app
      }
      removedApps.foreach(app => context.stop(context.child(app.name).get))
      newApps foreach createAppSupervisor
      modifiedApps foreach (app => context.child(app.name).get ! ConfigurationUpdated(app))
      context.become(configured(newConfiguration))
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
  private def createAppSupervisor(app: Application) = {
    val appSupervisor = context.actorOf(AppSupervisor(new File(workingDirectory, app.name)), app.name)
    appSupervisor ! ConfigurationUpdated(app)
    appSupervisor
  }
}

object LeadingActor {
  object Start
  object Stop
}
