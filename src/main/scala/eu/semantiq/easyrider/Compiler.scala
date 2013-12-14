package eu.semantiq.easyrider

import akka.actor._
import java.io.File
import scala.concurrent.duration.FiniteDuration
import eu.semantiq.easyrider.CommandRunner._
import akka.actor.Terminated

class Compiler(listener: ActorRef, workingDir: File, command: Option[String], timeout: FiniteDuration) extends Actor with ActorLogging {
  import Compiler._

  def waiting: Receive = {
    case Compile =>
      command match {
        case Some(command) =>
          val runner = context.actorOf(Props[CommandRunner], "compilation")
          runner ! Run("compilation", command, workingDir, false, timeout)
          context.become(compiling(runner))
        case None => listener ! CompilationSuccessful
      }
  }

  def compiling(commandRunner: ActorRef): Receive = {
    case Compile =>
      commandRunner ! Abort
      context.watch(commandRunner)
      context.become(restarting)
    case CommandExitCode(_, 0, _) => listener ! CompilationSuccessful
    case _: CommandCompleted => listener ! CompilationFailure
  }

  def restarting: Receive = {
    case Terminated =>
      context.become(waiting)
      self ! Compile
    case Compile => log.debug("received compilation request merged with previous one")
  }

  def receive: Receive = waiting
}

object Compiler {
  def apply(listener: ActorRef, workingDir: File, command: Some[String], timeout: FiniteDuration) = Props(classOf[Compiler],
    listener, workingDir, command, timeout)
  object CompilationSuccessful
  object CompilationFailure
  object Compile
}
