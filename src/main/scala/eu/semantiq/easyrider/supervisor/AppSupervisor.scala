package eu.semantiq.easyrider.supervisor

import java.io.{IOException, File}
import scala.concurrent.duration._

import akka.actor._
import akka.actor.SupervisorStrategy
import akka.event.LoggingReceive
import org.apache.commons.io.FileUtils

import eu.semantiq.easyrider.{PackageMetadata, AppRepository}

class AppSupervisor(app: String, repositoryRef: ActorRef, workingDirectory: File) extends Actor with ActorLogging with Stash {
  import AppSupervisor._

  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1.minute) {
    case _: IOException => SupervisorStrategy.Escalate
    case _ => SupervisorStrategy.Restart
  }

  override def preStart() {
    context.system.eventStream.subscribe(self, classOf[AppRepository.VersionAvailable])
    context.system.eventStream.subscribe(self, classOf[AppLifecycleCommand])
    log.debug("Subscribed to AppRepository update")
    workingDirectory.mkdir()
    logsDirectory.mkdir()
    repositoryRef ! AppRepository.GetVersionAvailable(app)
    context.setReceiveTimeout(2.minutes)
    context.actorOf(LogManager(logsDirectory), "log-manager")
  }

  val process = context.actorOf(ProcessWrapper(), "process-wrapper")
  var configuration: Option[Map[String, String]] = None
  var metadata: Option[PackageMetadata] = None
  var version: Option[String] = None
  var expectRunning = true

  def created: Receive = LoggingReceive {
    case AppRepository.VersionAvailable(newApp, newVersion) if app == newApp =>
      repositoryRef ! AppRepository.GetVersion(app, newVersion)
      version = Some(newVersion)
    case AppRepository.GetVersionResponse(_, newVersion, packageRef, newMetadata) if newVersion == version.get =>
      metadata = Some(newMetadata)
      packageRef.extractTo(new File(workingDirectory, newVersion))
      version = Some(newVersion)
      becomeRunningIfConfigured()
    case ConfigurationUpdated(newConfiguration) =>
      configuration = Some(newConfiguration)
      becomeRunningIfConfigured()
    case Start(newApp) if newApp == app =>
      expectRunning = true
      becomeRunningIfConfigured()
  }

  def running = LoggingReceive {
    case Stop(targetApp) if targetApp == app =>
      expectRunning = false
      process ! ProcessWrapper.Stop
      context.become(stopped)
      context.system.eventStream.publish(Stopped(app))
    case ConfigurationUpdated(newConfiguration) =>
      configuration = Some(newConfiguration)
      process ! createConfigurationUpdatedMessage
      process ! ProcessWrapper.Restart
      context.system.eventStream.publish(Started(app, version.get))
    case ReceiveTimeout => self ! DoMaintenance(app)
    case DoMaintenance(targetApp) if targetApp == app =>
      val removableVersions = workingDirectory.list().filter(_ != version.get)
      for(removableVersion <- removableVersions) {
        log.info("Garbadge collecting unused version: {}", removableVersion)
        FileUtils.deleteDirectory(new File(workingDirectory, removableVersion))
      }
    case AppRepository.VersionAvailable(newApp, newVersion) if newApp == app =>
      version = Some(newVersion)
      repositoryRef ! AppRepository.GetVersion(app, newVersion)
    case AppRepository.GetVersionResponse(_, newVersion, packageRef, newMetadata) if newVersion == version.get =>
      metadata = Some(newMetadata)
      packageRef.extractTo(new File(workingDirectory, newVersion))
      version = Some(newVersion)
      process ! createConfigurationUpdatedMessage
      process ! ProcessWrapper.Restart
      context.system.eventStream.publish(Started(app, newVersion))
    case ProcessWrapper.Crashed(exitCode) =>
      context.system.eventStream.publish(Crashed(app, exitCode))
      context.become(crashed)
  }

  def crashed = created

  def stopped = created

  def receive: Receive = created

  private val logsDirectory = new File(workingDirectory, "logs")

  private def createConfigurationUpdatedMessage = ProcessWrapper.ConfigurationUpdated(metadata.get.running.command, new File(workingDirectory, version.get), supervisorProvidedConfiguration ++ configuration.get)

  private def supervisorProvidedConfiguration = Map("supervisor.logs" -> logsDirectory.toString)

  private def becomeRunningIfConfigured() {
    for {
      config <- configuration
      newVersion <- version
      meta <- metadata if expectRunning
    } {
      context.become(running)
      context.system.eventStream.publish(Started(app, newVersion))
      process ! createConfigurationUpdatedMessage
      process ! ProcessWrapper.Start
    }
  }
}

object AppSupervisor {
  def apply(app: String, repositoryRef: ActorRef, workingDirectory: File) = Props(classOf[AppSupervisor], app, repositoryRef, workingDirectory)
  case class ConfigurationUpdated(settings: Map[String, String])
  sealed trait AppLifecycleEvent {
    def app: String
  }
  case class Started(app: String, version: String) extends  AppLifecycleEvent
  case class Stopped(app: String) extends AppLifecycleEvent
  case class Crashed(app: String, errorCode: Int) extends AppLifecycleEvent

  sealed trait AppLifecycleCommand {
    def app: String
  }
  case class Start(app: String) extends AppLifecycleCommand
  case class Stop(app: String) extends AppLifecycleCommand
  case class Restart(app: String) extends AppLifecycleCommand
  case class DoMaintenance(app: String) extends AppLifecycleCommand
}
