package eu.semantiq.easyrider.supervisor

import akka.actor.{Stash, ActorLogging, Actor}
import java.io.File
import scala.sys.process._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.util.{Failure, Success}
import akka.event.LoggingReceive

class ProcessWrapper(dir: File) extends Actor with ActorLogging with Stash {
  // TODO: implement postStop
  import ProcessWrapper._

  def created: Receive = {
    case ConfigurationUpdated(command) => context.become(stopped(command))
  }

  def stopped(command: String): Receive = LoggingReceive {
    case Start =>
      val p = Process(command, dir, "PATH" -> System.getenv("PATH")).run(ProcessLogger(m => log.debug("process: {}", m)))
      context.become(started(command, p))
      Future(p.exitValue()) onComplete  {
        case Success(code) => self ! ProcessStopped(code)
        case Failure(e) => self ! e
      }
  }

  def started(command: String, process: Process): Receive = {
    case Stop =>
      terminate(process)
      context.become(stopping(command))
    case Restart =>
      restart(command, process)
    case ConfigurationUpdated(newCommand) =>
      context.become(started(newCommand, process))
    case event: ProcessStopped =>
      log.error("Process {} died unexpectedly with exit code {}", command, event.exitCode)
      context.parent ! event
      context.become(stopped(command))
  }

  def stopping(command: String): Receive = {
    case event: ProcessStopped =>
      context.parent ! event
      context.become(stopped(command))
      unstashAll()
    case Start => stash()
  }

  def receive = created

  private def terminate(process: Process) = process.destroy()
  private def restart(command: String, process: Process) {
    terminate(process)
    context.become(stopping(command))
    self ! Start
  }
}

object ProcessWrapper {
  object Start
  object Stop
  object Restart
  case class ConfigurationUpdated(command: String)
  case class ProcessStopped(exitCode: Int)
}
