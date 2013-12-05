package eu.semantiq.easyrider

import akka.actor.{ActorLogging, Props, ActorRef, Actor}
import java.io.File
import akka.event.LoggingReceive
import scala.concurrent.duration._

class AppSupervisor(app: Application) extends Actor with ActorLogging {
  import AppSupervisor._

  def created: Receive = {
    case Start => {
      val git = context.actorOf(Props(classOf[GitWorkingCopy], self, app.name, app.repository, new File("working")), "repository")
      git ! GitWorkingCopy.Activate
      context.become(preparing(git))
    }
  }

  def preparing(git: ActorRef): Receive = {
    case WorkingCopyUpdated => app.commands.compile match {
      case None => becomeRunning(git)
      case Some(command) =>
        val compilation = context.actorOf(Props[CommandRunner], "compilation")
        compilation ! CommandRunner.Run("compilation", command, new File(s"working/${app.name}"), timeout = 2.minutes)
        context.become(compiling(git, compilation))
    }
    case GitCloneFailed => context.stop(self)
  }

  def compiling(git: ActorRef, compilation: ActorRef): Receive = {
    case CommandRunner.CommandExitCode("compilation", 0, _) => becomeRunning(git)
    case CommandRunner.CommandExitCode("compilation", _, _) =>
      log.error("compilation failed")
      context.become(preparing(git))
    case CommandRunner.CommandTimedOut =>
      log.error("compilation timed-out")
      context.become(preparing(git))
    case WorkingCopyUpdated =>
      log.info("new version available - aborting compilation")
      compilation ! CommandRunner.Abort
      self ! WorkingCopyUpdated
      context.become(preparing(git))
  }

  def running(git: ActorRef, process: ActorRef) = LoggingReceive {
    case ProcessWrapper.ProcessStopped(code) =>
      log.error("App crashed with code {}", code)
      context.stop(process)
      context.become(preparing(git))
    case WorkingCopyUpdated =>
      log.info("new version available")
      process ! ProcessWrapper.Stop
      process ! ProcessWrapper.Start
  }

  def receive: Actor.Receive = created

  private def becomeRunning(git: ActorRef) {
    log.info("ready to rock")
    val settingsString = app.settings.map {
      case (key, value) => s"-D$key=$value"
    } mkString(" ")
    val process = context.actorOf(Props(classOf[ProcessWrapper], app.commands.run + " " + settingsString, new File(s"working/${app.name}")), "processWrapper")
    process ! ProcessWrapper.Start
    context.become(running(git, process))
  }
}

object AppSupervisor {
  object Start
  object WorkingCopyUpdated
  object GitCloneFailed
}
