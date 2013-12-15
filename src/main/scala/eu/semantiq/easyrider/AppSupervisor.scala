package eu.semantiq.easyrider

import akka.actor._
import java.io.File
import akka.event.LoggingReceive
import scala.concurrent.duration._
import eu.semantiq.easyrider.{Application=>EasyRiderApplication}

class AppSupervisor extends Actor with ActorLogging with Stash {
  import AppSupervisor._

  var app: EasyRiderApplication = _

  def created: Receive = {
    case ConfigurationUpdated(configuration) => {
      app = configuration
      val git = context.actorOf(Props(classOf[GitWorkingCopy], self, app.name, app.repository, new File("working")), "repository")
      git ! GitWorkingCopy.Activate
      context.become(preparing(git))
    }
  }

  def preparing(git: ActorRef): Receive = {
    case WorkingCopyUpdated =>
      context.system.eventStream.publish(Updated(app.name, "TODO"))
      app.commands.compile match {
        case None => becomeRunning(git)
        case Some(command) => becomeCompiling(command, git)
      }
    case GitCloneFailed => context.stop(self)
  }


  def compiling(git: ActorRef, compilation: ActorRef): Receive = {
    case CommandRunner.CommandExitCode("compilation", 0, _) =>
      context.system.eventStream.publish(Compiled(app.name, "TODO"))
      unstashAll()
      becomeRunning(git)
    case CommandRunner.CommandExitCode("compilation", _, _) =>
      log.error("compilation failed")
      context.become(preparing(git))
    case CommandRunner.CommandTimedOut =>
      log.error("compilation timed-out")
      context.become(preparing(git))
    case WorkingCopyUpdated =>
      log.info("new version available, but will wait until current compilation is finished")
      stash()
  }

  def running(git: ActorRef, process: ActorRef) = LoggingReceive {
    case ProcessWrapper.ProcessStopped(code) =>
      log.error("App crashed with code {}", code)
      context.stop(process)
      context.become(preparing(git))
    case WorkingCopyUpdated =>
      log.info("new version available")
      process ! ProcessWrapper.Stop
      app.commands.compile match {
        case Some(command) => becomeCompiling(command, git)
        case None => context.become(restarting(git))
      }
  }

  def restarting(git: ActorRef) = LoggingReceive {
    case ProcessWrapper.ProcessStopped(code) =>
      log.info("App stopped - starting again")
      becomeRunning(git)
  }

  def receive: Actor.Receive = created

  private def becomeRunning(git: ActorRef) {
    log.info("ready to rock")
    context.system.eventStream.publish(Started(app.name, "TODO"))
    val settingsString = app.settings.map {
      case (key, value) => s"-D$key=$value"
    } mkString(" ")
    val process = context.actorOf(Props(classOf[ProcessWrapper], app.commands.run + " " + settingsString, new File(s"working/${app.name}")), "processWrapper")
    process ! ProcessWrapper.Start
    context.become(running(git, process))
  }

  private def becomeCompiling(command: String, git: ActorRef) {
    val compilation = context.actorOf(Props[CommandRunner], "compilation")
    compilation ! CommandRunner.Run("compilation", command, new File(s"working/${app.name}"), timeout = 10.minutes)
    context.become(compiling(git, compilation))
  }
}

object AppSupervisor {
  def apply() = Props(classOf[AppSupervisor])
  case class ConfigurationUpdated(app: EasyRiderApplication)
  object Start
  object WorkingCopyUpdated
  object GitCloneFailed
  sealed trait AppLifecycleEvent
  case class Updated(app: String, rev: String) extends AppLifecycleEvent
  case class Compiled(app: String, rev: String) extends AppLifecycleEvent
  case class Started(app: String, rev: String) extends  AppLifecycleEvent
}
