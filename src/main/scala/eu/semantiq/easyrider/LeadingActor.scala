package eu.semantiq.easyrider

import akka.actor._
import java.io.File
import scala.concurrent.duration._
import eu.semantiq.easyrider.supervisor.AppSupervisor
import eu.semantiq.easyrider.console.HttpDispatcher
import eu.semantiq.easyrider.builder.AppBuilder
import org.apache.commons.io.FileUtils
import akka.event.LoggingReceive

class LeadingActor(configFileLocation: File, workingDirectory: File, configurationPollingInterval: FiniteDuration) extends Actor with Stash with ActorLogging {
  workingDirectory.mkdir()

  context.actorOf(ConfigurationManager(self, configFileLocation, configurationPollingInterval), "configuration-manager")
  private val statusMonitor = context.actorOf(Props[StatusMonitor], "status-monitor")
  private val dispatcher = context.actorOf(HttpDispatcher(statusMonitor), "http-dispatcher")
  private val repository = context.actorOf(AppRepository(new File(workingDirectory, "repository")), "repository")

  private var supervisors = Map[String, ActorRef]()
  private var builders = Map[String, ActorRef]()

  dispatcher ! HttpDispatcher.NewConfiguration(8100)

  def running(configuration: Seq[Application]): Receive = LoggingReceive {
    case ConfigurationManager.Reconfigured(newConfiguration) =>
      val createdApps = newConfiguration.filterNot(app => configuration.exists(_.name == app.name))
      val removedApps = configuration.filterNot(app => newConfiguration.exists(_.name == app.name))
      val updatedApps = newConfiguration.filter { app =>
        val oldApp = configuration.find(_.name == app.name)
        oldApp.isDefined && oldApp.get != app
      }
      removedApps foreach removeApp
      createdApps foreach createApp
      updatedApps foreach updateApp
      context.become(running(newConfiguration))
  }

  def receive: Receive = running(Seq.empty)

  private def createApp(app: Application) {
    val builder = context.actorOf(AppBuilder(app.name, repository, builderWorkingDirectory(app)), s"${app.name}-builder")
    builders += (app.name -> builder)
    val supervisor = context.actorOf(AppSupervisor(app.name, repository, supervisorWorkingDirectory(app)), s"${app.name}-supervisor")
    supervisors += (app.name -> supervisor)
    builder ! AppBuilder.ConfigurationUpdated(app.repository)
    supervisor ! AppSupervisor.ConfigurationUpdated(app.settings)
  }

  private def supervisorWorkingDirectory(app: Application) = new File(workingDirectory, s"${app.name}-supervisor")
  private def builderWorkingDirectory(app: Application) = new File(workingDirectory, s"${app.name}-builder")

  private def removeApp(app: Application) {
    log.info(s"Removing ${app.name} as it is no longer in configuration")
    val builder = builders(app.name)
    val supervisor = supervisors(app.name)
    context.stop(builder)
    context.stop(supervisor)
    FileUtils.deleteDirectory(builderWorkingDirectory(app))
    FileUtils.deleteDirectory(supervisorWorkingDirectory(app))
    builders -= app.name
    supervisors -= app.name
  }

  private def updateApp(app: Application) {
    builders(app.name) ! AppBuilder.ConfigurationUpdated(app.repository)
    supervisors(app.name) ! AppSupervisor.ConfigurationUpdated(app.settings)
  }
}

object LeadingActor {
  def apply(configFileLocation: File, workingDirectory: File, configurationPollingInterval: FiniteDuration = 30.seconds) = Props(classOf[LeadingActor], configFileLocation, workingDirectory, configurationPollingInterval)
}
