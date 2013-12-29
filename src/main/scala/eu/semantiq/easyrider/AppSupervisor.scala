package eu.semantiq.easyrider

import akka.actor._
import java.io.File
import akka.event.LoggingReceive
import scala.concurrent.duration._
import eu.semantiq.easyrider.{Application=>EasyRiderApplication}
import eu.semantiq.easyrider.builder.GitWorkingCopy

class AppSupervisor(workingDirectory: File, pullFrequency: FiniteDuration, compilationTimeout: FiniteDuration) extends Actor with ActorLogging with Stash {
  import AppSupervisor._

  var app: EasyRiderApplication = _

  workingDirectory.mkdir()
  val repoDirectory = new File(workingDirectory, "appRepo")
  val git = context.actorOf(GitWorkingCopy(self, repoDirectory, pullFrequency), "repository")
  val compiler = context.actorOf(Compiler(self, repoDirectory, compilationTimeout), "compiler")
  val process = context.actorOf(Props(classOf[ProcessWrapper], repoDirectory), "process-wrapper")
  context.system.eventStream.subscribe(self, classOf[AppLifecycleCommand])

  def created: Receive = {
    case ConfigurationUpdated(configuration) =>
      app = configuration
      git ! GitWorkingCopy.ConfigurationUpdated(configuration.repository)
      process ! ProcessWrapper.ConfigurationUpdated(createCommandLine)
      context.become(preparing)
  }

  def preparing: Receive = {
    case WorkingCopyUpdated =>
      context.system.eventStream.publish(Updated(app.name, "TODO"))
      compiler ! Compiler.Compile(app.compilation.command)
      context.become(compiling)
      unstashAll()
    case GitCloneFailed => context.stop(self)
    case _: ConfigurationUpdated => stash() // TODO: compilation failure may happen due to a configuration problem, therefore we need to distinguish between not having a working copy and waiting to resolve a compilation failure
  }

  def compilationFailure: Receive = {
    case WorkingCopyUpdated =>
    case ConfigurationUpdated(configurationUpdated) =>
  }

  def compiling: Receive = {
    case Compiler.CompilationSuccessful =>
      context.system.eventStream.publish(Compiled(app.name, "TODO"))
      unstashAll()
      becomeRunning()
    case Compiler.CompilationFailure =>
      unstashAll()
      context.become(preparing)
    case WorkingCopyUpdated =>
      log.info("new version available, but will wait until current compilation is finished")
      stash()
    case _: ConfigurationUpdated => stash()
  }

  def running(process: ActorRef) = LoggingReceive {
    case ProcessWrapper.ProcessStopped(code) =>
      log.error("App crashed with code {}", code)
      context.system.eventStream.publish(Stopped(app.name))
      context.stop(process)
      context.become(preparing)
    case WorkingCopyUpdated =>
      log.info("new version available")
      becomeCompiling(process)
    case Stop(targetApp) if targetApp == app.name =>
      process ! ProcessWrapper.Stop
      context.system.eventStream.publish(Stopped(app.name))
      context.become(stopped)
    case ConfigurationUpdated(newConfiguration) =>
      val oldConfiguration = app
      app = newConfiguration
      if (oldConfiguration.repository != newConfiguration.repository) {
        git ! GitWorkingCopy.ConfigurationUpdated(newConfiguration.repository)
      } else if (oldConfiguration.compilation != newConfiguration.compilation) {
        becomeCompiling(process)
      } else {
        process ! ProcessWrapper.ConfigurationUpdated(newConfiguration.running.command)
      }
  }

  def stopped: Receive = {
    case Start(targetApp) if targetApp == app.name =>
      becomeRunning()
  }

  def receive: Receive = created

  private def becomeRunning() {
    log.info("ready to rock")
    context.system.eventStream.publish(Started(app.name, "TODO"))
    val process = context.actorOf(Props(classOf[ProcessWrapper], createCommandLine, repoDirectory), "processWrapper")
    process ! ProcessWrapper.Start
    context.become(running(process))
  }

  private def createCommandLine: String = {
    val settingsString = app.running.settings.map {
      case (key, value) => s"-D$key=$value"
    } mkString " "
    app.running.command + " " + settingsString
  }

  private def becomeCompiling(process: ActorRef) {
    process ! ProcessWrapper.Stop
    compiler ! Compiler.Compile(app.compilation.command)
    context.become(compiling)
  }
}

object AppSupervisor {
  def apply(workingDirectory: File, pullFrequency: FiniteDuration = 30.seconds, compilationTimeout: FiniteDuration = 5.minutes) =
    Props(classOf[AppSupervisor], workingDirectory, pullFrequency, compilationTimeout)
  case class ConfigurationUpdated(app: EasyRiderApplication)
  object WorkingCopyUpdated
  object GitCloneFailed
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
