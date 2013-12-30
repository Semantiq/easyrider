package eu.semantiq.easyrider.builder

import akka.actor._
import java.io.File
import scala.concurrent.duration.FiniteDuration
import eu.semantiq.easyrider.CommandRunner._
import akka.actor.Terminated
import akka.event.LoggingReceive
import eu.semantiq.easyrider.CommandRunner

class Compiler(listener: ActorRef, workingDir: File, timeout: FiniteDuration) extends Actor with ActorLogging {
  import Compiler._

  def waiting: Receive = LoggingReceive {
    case Compile(Some(command)) =>
          val runner = context.actorOf(Props[CommandRunner], "compilation")
          runner ! Run("compilation", command, workingDir, collectOutput = false, timeout)
          context.become(compiling(runner))
    case Compile(None) => listener ! CompilationSuccessful
  }

  def compiling(commandRunner: ActorRef): Receive = LoggingReceive {
    case Compile(command) =>
      commandRunner ! Abort
      context.watch(commandRunner)
      context.become(restarting(command))
    case CommandExitCode(_, 0, _) => listener ! CompilationSuccessful
    case _: CommandCompleted => listener ! CompilationFailure
  }

  def restarting(command: Option[String]): Receive = LoggingReceive {
    case _: Terminated =>
      context.become(waiting)
      self ! Compile(command)
    case Compile(newCommand) =>
      log.debug("received compilation request replaced previous one")
      context.become(restarting(newCommand))
  }

  def receive: Receive = waiting
}

object Compiler {
  def apply(listener: ActorRef, workingDir: File, timeout: FiniteDuration) = Props(classOf[Compiler],
    listener, workingDir, timeout)
  object CompilationSuccessful
  object CompilationFailure
  case class Compile(command: Option[String])
}
