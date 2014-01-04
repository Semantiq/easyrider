package eu.semantiq.easyrider.supervisor

import akka.actor.{Props, Stash, ActorLogging, Actor}
import java.io.File
import scala.sys.process._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.util.{Failure, Success}
import akka.event.LoggingReceive

class ProcessWrapper extends Actor with ActorLogging with Stash {
  // TODO: implement postStop
  import ProcessWrapper._

  def created: Receive = {
    case configuration: ConfigurationUpdated => context.become(stopped(configuration))
  }

  def stopped(configuration: ConfigurationUpdated): Receive = LoggingReceive {
    case Start =>
      val environment = configuration.settings + ("PATH" -> System.getenv("PATH"))
      val p = Process(configuration.command, configuration.workingDirectory, environment.toSeq :_*).run(ProcessLogger(m => log.debug("process: {}", m)))
      context.become(started(configuration, p))
      Future(p.exitValue()) onComplete  {
        case Success(code) => self ! ProcessStopped(code)
        case Failure(e) => self ! e
      }
  }

  def started(configuration: ConfigurationUpdated, process: Process): Receive = {
    case Stop =>
      terminate(process)
      context.become(stopping(configuration))
    case Restart =>
      restart(configuration, process)
    case newConfiguration: ConfigurationUpdated =>
      context.become(started(newConfiguration, process))
    case event: ProcessStopped =>
      log.error("Process {} died unexpectedly with exit code {}", configuration, event.exitCode)
      context.parent ! event
      context.become(stopped(configuration))
  }

  def stopping(configuration: ConfigurationUpdated): Receive = {
    case event: ProcessStopped =>
      context.parent ! event
      context.become(stopped(configuration))
      unstashAll()
    case Start => stash()
  }

  def receive = created

  private def terminate(process: Process) = process.destroy()
  private def restart(configuration: ConfigurationUpdated, process: Process) {
    terminate(process)
    context.become(stopping(configuration))
    self ! Start
  }
}

object ProcessWrapper {
  def apply() = Props(classOf[ProcessWrapper])

  object Start
  object Stop
  object Restart
  case class ConfigurationUpdated(command: String, workingDirectory: File, settings: Map[String, String])
  case class ProcessStopped(exitCode: Int)
}
