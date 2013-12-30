package eu.semantiq.easyrider

import akka.actor._
import java.io.File
import akka.event.LoggingReceive
import scala.concurrent.duration._
import eu.semantiq.easyrider.{Application => EasyRiderApplication}

class AppSupervisor(workingDirectory: File, pullFrequency: FiniteDuration, compilationTimeout: FiniteDuration) extends Actor with ActorLogging with Stash {
  import AppSupervisor._

  var app: EasyRiderApplication = _

  workingDirectory.mkdir()

  def created: Receive = {
    case ConfigurationUpdated(configuration) =>
      app = configuration
      ???
  }

  def running(process: ActorRef) = LoggingReceive {
    case Stop(targetApp) if targetApp == app.name => ???
    case ConfigurationUpdated(newConfiguration) => ???
  }

  def stopped: Receive = {
    case Start(targetApp) if targetApp == app.name => ???
  }

  def receive: Receive = created

}

object AppSupervisor {
  def apply(workingDirectory: File, pullFrequency: FiniteDuration = 30.seconds, compilationTimeout: FiniteDuration = 5.minutes) =
    Props(classOf[AppSupervisor], workingDirectory, pullFrequency, compilationTimeout)
  case class ConfigurationUpdated(app: EasyRiderApplication)
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
