package eu.semantiq.easyrider.supervisor

import akka.actor._
import java.io.File
import akka.event.LoggingReceive
import scala.concurrent.duration._
import eu.semantiq.easyrider.{PackageMetadata, AppRepository}

class AppSupervisor(app: String, repositoryRef: ActorRef, workingDirectory: File) extends Actor with ActorLogging with Stash {
  import AppSupervisor._

  override def preStart() {
    context.system.eventStream.subscribe(self, classOf[AppRepository.VersionAvailable])
    log.debug("Subscribed to AppRepository update")
    workingDirectory.mkdir()
    repositoryRef ! AppRepository.GetVersionAvailable(app)
  }

  var configuration: Option[Map[String, String]] = None
  var metadata: Option[PackageMetadata] = None
  var version: Option[String] = None

  def created: Receive = LoggingReceive {
    case AppRepository.VersionAvailable(newApp, newVersion) if app == newApp =>
      sender ! AppRepository.GetVersion(app, newVersion)
      version = Some(newVersion)
    case AppRepository.GetVersionResponse(_, newVersion, packageRef, newMetadata) if newVersion == version.get =>
      metadata = Some(newMetadata)
      packageRef.extractTo(new File(workingDirectory, newVersion))
      version = Some(newVersion)
      becomeRunningIfConfigured()
    case ConfigurationUpdated(newConfiguration) =>
      configuration = Some(newConfiguration)
      becomeRunningIfConfigured()
  }

  def running(process: ActorRef) = LoggingReceive {
    case Stop(targetApp) if targetApp == app =>
      process ! ProcessWrapper.Stop
      context.become(stopped)
    case ConfigurationUpdated(newConfiguration) =>
      configuration = Some(newConfiguration)
      process ! createConfigurationUpdatedMessage
      process ! ProcessWrapper.Restart
    case AppRepository.VersionAvailable(newApp, newVersion) if newApp == app =>
      version = Some(newVersion)
      repositoryRef ! AppRepository.GetVersion(app, newVersion)
    case AppRepository.GetVersionResponse(_, newVersion, packageRef, newMetadata) if newVersion == version.get =>
      metadata = Some(newMetadata)
      packageRef.extractTo(new File(workingDirectory, newVersion))
      version = Some(newVersion)
      process ! createConfigurationUpdatedMessage
      process ! ProcessWrapper.Restart
  }

  def stopped: Receive = {
    case Start(targetApp) if targetApp == app => ???
  }

  def receive: Receive = created

  def createConfigurationUpdatedMessage = ProcessWrapper.ConfigurationUpdated(metadata.get.running.command, new File(workingDirectory, version.get), configuration.get)

  private def becomeRunningIfConfigured() {
    for {
      config <- configuration
      newVersion <- version
      meta <- metadata
    } {
      val process = context.actorOf(ProcessWrapper(), "process-wrapper")
      context.become(running(process))
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
  case class Updated(app: String, rev: String) extends AppLifecycleEvent
  case class Compiled(app: String, rev: String) extends AppLifecycleEvent
  case class Started(app: String, rev: String) extends  AppLifecycleEvent
  case class Stopped(app: String) extends AppLifecycleEvent

  sealed trait AppLifecycleCommand {
    def app: String
  }
  case class Start(app: String) extends AppLifecycleCommand
  case class Stop(app: String) extends AppLifecycleCommand
}
