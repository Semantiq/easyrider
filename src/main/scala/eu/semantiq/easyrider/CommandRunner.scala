package eu.semantiq.easyrider

import akka.actor.{ActorLogging, Actor}
import java.io.File
import scala.sys.process.{ProcessLogger, Process}
import scala.concurrent.duration.{FiniteDuration, Duration}
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future
import scala.util.{Success, Failure}
import akka.event.LoggingReceive

class CommandRunner extends Actor with ActorLogging {
  import CommandRunner._

  def await: Receive = {
    case Run(id, command, dir, collectOutput, timeout) =>
      log.info(s"Running ${command} in ${dir}")
      val logger = collectOutput match {
        case true => ProcessLogger(m => self ! Line(m), m => log.debug(s"$command: $m"))
        case false => ProcessLogger(m => log.debug(s"$command: $m"))
      }
      val p = Process(command, dir, "PATH" -> System.getenv("PATH")).run(logger)
      context.become(running(id, p, new StringBuilder))
      Future(p.exitValue()) onComplete {
        case Success(code) => self ! Completed(code)
        case Failure(e) => throw e // FIXME: handle this in the actor
      }
      if (timeout.isFinite) {
        context.system.scheduler.scheduleOnce(timeout.asInstanceOf[FiniteDuration]) {
          self ! Timeout
        }
      }
  }

  def running(id: String, p: Process, output: StringBuilder): Receive = LoggingReceive {
    case Completed(code) =>
      context.parent ! CommandExitCode(id, code, if(output.size == 0) None else Some(output.toString))
      context.stop(self)
    case Line(s) => output.append(s); output.append("\n")
    case Timeout =>
      p.destroy()
      context.parent ! CommandTimedOut(id)
      context.stop(self)
    case Abort =>
      p.destroy()
      context.stop(self)
  }

  def receive = await
}

object CommandRunner {
  case class Run(id: String, command: String, dir: File, collectOutput: Boolean = false,
                 timeout: Duration = Duration.Undefined)
  object Abort
  sealed trait CommandCompleted {
    def id: String
  }
  case class CommandExitCode(id: String, exitCode: Int, output: Option[String]) extends CommandCompleted
  case class CommandTimedOut(id: String) extends CommandCompleted
  private case class Completed(exitCode: Int)
  private object Timeout
  private case class Line(string: String)
}
