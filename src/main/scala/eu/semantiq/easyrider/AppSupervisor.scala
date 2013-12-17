package eu.semantiq.easyrider

import akka.actor._
import java.io.File
import akka.event.LoggingReceive
import scala.concurrent.duration._
import eu.semantiq.easyrider.{Application=>EasyRiderApplication}

class AppSupervisor(workingDirectory: File, pullFrequency: FiniteDuration, compilationTimeout: FiniteDuration) extends Actor with ActorLogging with Stash {
  import AppSupervisor._

  var app: EasyRiderApplication = _

  workingDirectory.mkdir()
  val repoDirectory = new File(workingDirectory, "repo")
  val git = context.actorOf(GitWorkingCopy(self, repoDirectory, pullFrequency), "repository")
  val compiler = context.actorOf(Compiler(self, repoDirectory, compilationTimeout), "compiler")

  def created: Receive = {
    case ConfigurationUpdated(configuration) =>
      app = configuration
      git ! GitWorkingCopy.ConfigurationUpdated(configuration.repository)
      context.become(preparing)
  }

  def preparing: Receive = {
    case WorkingCopyUpdated =>
      context.system.eventStream.publish(Updated(app.name, "TODO"))
      compiler ! Compiler.Compile(app.commands.compile)
      context.become(compiling)
    case GitCloneFailed => context.stop(self)
  }

  def compiling: Receive = {
    case Compiler.CompilationSuccessful =>
      context.system.eventStream.publish(Compiled(app.name, "TODO"))
      unstashAll()
      becomeRunning()
    case Compiler.CompilationFailure =>
      context.become(preparing)
    case WorkingCopyUpdated =>
      log.info("new version available, but will wait until current compilation is finished")
      stash()
  }

  def running(process: ActorRef) = LoggingReceive {
    case ProcessWrapper.ProcessStopped(code) =>
      log.error("App crashed with code {}", code)
      context.stop(process)
      context.become(preparing)
    case WorkingCopyUpdated =>
      log.info("new version available")
      process ! ProcessWrapper.Stop
      compiler ! Compiler.Compile
      context.become(compiling)
  }

  def receive: Actor.Receive = created

  private def becomeRunning() {
    log.info("ready to rock")
    context.system.eventStream.publish(Started(app.name, "TODO"))
    val settingsString = app.settings.map {
      case (key, value) => s"-D$key=$value"
    } mkString " "
    val process = context.actorOf(Props(classOf[ProcessWrapper], app.commands.run + " " + settingsString, new File(s"working/${app.name}")), "processWrapper")
    process ! ProcessWrapper.Start
    context.become(running(process))
  }
}

object AppSupervisor {
  def apply(workingDirectory: File, pullFrequency: FiniteDuration = 30.seconds, compilationTimeout: FiniteDuration = 5.minutes) =
    Props(classOf[AppSupervisor], workingDirectory, pullFrequency, compilationTimeout)
  case class ConfigurationUpdated(app: EasyRiderApplication)
  object WorkingCopyUpdated
  object GitCloneFailed
  sealed trait AppLifecycleEvent
  case class Updated(app: String, rev: String) extends AppLifecycleEvent
  case class Compiled(app: String, rev: String) extends AppLifecycleEvent
  case class Started(app: String, rev: String) extends  AppLifecycleEvent
}
