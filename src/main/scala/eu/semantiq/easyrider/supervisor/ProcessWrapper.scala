package eu.semantiq.easyrider.supervisor

import akka.actor.{Props, Stash, ActorLogging, Actor}
import java.io.File
import scala.sys.process._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.util.{Failure, Success}
import akka.event.LoggingReceive

class ProcessWrapper extends Actor with ActorLogging with Stash {
  import ProcessWrapper._

  var process: Option[Process] = None

  override def postStop() {
    log.info(s"Terminating process: ${process.isDefined}")
    process foreach terminate
  }

  def created: Receive = {
    case configuration: ConfigurationUpdated => context.become(stopped(configuration))
  }

  def stopped(configuration: ConfigurationUpdated): Receive = LoggingReceive {
    case Start =>
      val environment = configuration.settings + ("PATH" -> System.getenv("PATH"))
      val augmentedCommand = configuration.settings.foldLeft(configuration.command) {
        case (command, (name, value)) => command.replace("$" + name, value)
      }
      log.info("Starting process with command line '{}', environment {}", augmentedCommand, environment)
      val p = Process(augmentedCommand, configuration.workingDirectory, environment.toSeq :_*).run(ProcessLogger(m => log.debug("process: {}", m)))
      process = Some(p)
      context.become(started(configuration))
      Future(p.exitValue()) onComplete  {
        case Success(code) => self ! ProcessStopped(code)
        case Failure(e) => self ! e
      }
    case newConfiguration: ConfigurationUpdated => context.become(stopped(newConfiguration))
  }

  def started(configuration: ConfigurationUpdated): Receive = {
    case Stop =>
      terminate(process.get)
      context.become(stopping(configuration))
    case Restart =>
      restart(configuration)
    case newConfiguration: ConfigurationUpdated =>
      context.become(started(newConfiguration))
    case event: ProcessStopped =>
      log.error("Process {} died unexpectedly with exit code {}", configuration, event.exitCode)
      context.parent ! Crashed(event.exitCode)
      context.parent ! event
      context.become(stopped(configuration))
      process = None
  }

  def stopping(configuration: ConfigurationUpdated): Receive = {
    case event: ProcessStopped =>
      context.parent ! event
      context.become(stopped(configuration))
      process = None
      unstashAll()
    case Start => stash()
  }

  def receive = created

  private def restart(configuration: ConfigurationUpdated) {
    terminate(process.get)
    context.become(stopping(configuration))
    self ! Start
  }

  private def terminate(process: Process) = process.destroy()

}

object ProcessWrapper {
  def apply() = Props(classOf[ProcessWrapper])

  object Start
  object Stop
  object Restart
  case class Crashed(exitCode: Int)
  case class ConfigurationUpdated(command: String, workingDirectory: File, settings: Map[String, String])
  case class ProcessStopped(exitCode: Int)
}
