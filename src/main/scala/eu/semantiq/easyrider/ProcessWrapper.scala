package eu.semantiq.easyrider

import akka.actor.{ActorLogging, Actor}
import java.io.File
import scala.sys.process._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.util.{Failure, Success}
import akka.event.LoggingReceive

class ProcessWrapper(command: String, dir: File) extends Actor with ActorLogging {
  import ProcessWrapper._

  def stopped: Receive = LoggingReceive {
    case Start =>
      val p = Process(command, dir, "PATH" -> System.getenv("PATH")).run(ProcessLogger(m => log.debug("process: {}", m)))
      context.become(started(p))
      Future(p.exitValue) onComplete  {
        case Success(code) =>
          context.parent ! ProcessStopped(code)
          context.become(stopped)
        case Failure(e) => throw e
      }
  }

  def started(process: Process) = LoggingReceive {
    case Stop =>
      process.destroy()
      context.become(stopped)
  }

  def receive = stopped
}

object ProcessWrapper {
  object Start
  object Stop
  case class ProcessStopped(exitCode: Int)
}