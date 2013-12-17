package eu.semantiq.easyrider

import akka.actor.{Cancellable, ActorLogging, Actor}
import java.io.File
import scala.sys.process.{ProcessLogger, Process}
import scala.concurrent.duration.{FiniteDuration, Duration}
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future
import scala.util.{Success, Failure}
import akka.event.LoggingReceive
import akka.pattern.PipeToSupport

class CommandRunner extends Actor with ActorLogging with PipeToSupport {
  import CommandRunner._

  def await: Receive = {
    case Run(id, command, dir, collectOutput, timeout) =>
      log.info(s"Running $command in $dir")
      val logger = collectOutput match {
        case true => ProcessLogger(m => self ! Line(m), m => log.debug(s"$command: $m"))
        case false => ProcessLogger(m => log.debug(s"$command: $m"))
      }
      val p = Process(command, dir, "PATH" -> System.getenv("PATH")).run(logger)
      Future(p.exitValue()).map(code => Completed(code)).recover { case e: Exception => Failure(e) } pipeTo self
      val timeoutSubscription = asFiniteDurationOption(timeout) map { duration =>
        context.system.scheduler.scheduleOnce(timeout.asInstanceOf[FiniteDuration]) {
          self ! Timeout
        }
      }
      context.become(running(id, p, new StringBuilder, timeoutSubscription))
  }

  def running(id: String, p: Process, output: StringBuilder, timeoutSubscription: Option[Cancellable]): Receive = LoggingReceive {
    case Completed(code) =>
      context.parent ! CommandExitCode(id, code, if(output.size == 0) None else Some(output.toString()))
      timeoutSubscription.foreach(_.cancel())
      context.stop(self)
    case Line(s) => output.append(s); output.append("\n")
    case Timeout =>
      p.destroy()
      context.parent ! CommandTimedOut(id)
      context.stop(self)
    case Abort =>
      p.destroy()
      timeoutSubscription.foreach(_.cancel())
      context.stop(self)
    case Failure(e) => throw e
  }

  def receive = await

  private def asFiniteDurationOption(duration: Duration) = if (duration.isFinite()) Some(duration.asInstanceOf[FiniteDuration]) else None
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
