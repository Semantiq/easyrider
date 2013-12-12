package eu.semantiq.easyrider

import java.io.File
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._
import akka.actor.{ActorRef, Props, ActorLogging, Actor}
import akka.event.LoggingReceive
import akka.pattern.PipeToSupport

class GitWorkingCopy(listener: ActorRef, name: String, repository: GitRepositoryRef, workingDirectory: File)
  extends Actor with ActorLogging with PipeToSupport {
  import GitWorkingCopy._
  import CommandRunner._

  val pullSchedule = context.system.scheduler.schedule(10.seconds, 30.seconds, self, Pull)
  workingDirectory.mkdir()

  override def postStop() = pullSchedule.cancel

  def passive = LoggingReceive {
    case Activate =>
      isCloned map {
        case true => CloneComplete
        case false => Clone
      } pipeTo self
      context.become(activating)
  }

  def activating: Receive = LoggingReceive {
    case Clone => cloneRepo
    case CommandExitCode("clone", 0, _) => self ! CloneComplete
    case CloneComplete => checkout
    case CommandExitCode("checkout", 0, _) => self ! CheckoutComplete
    case CheckoutComplete => getRevision
    case CommandExitCode("get-revision", 0, Some(revision)) =>
       context.become(active(revision))
       listener ! AppSupervisor.WorkingCopyUpdated
    case failure: CommandCompleted => throw new RuntimeException("Command execution failed: " + failure)
  }

  def active(revision: String): Receive = {
    case Pull => pullRepo
    case CommandExitCode("pull", 0, _) => getRevision
    case CommandExitCode("get-revision", 0, Some(newRevision)) => if (newRevision != revision) {
      context.become(active(newRevision))
      listener ! AppSupervisor.WorkingCopyUpdated
    }
    case failure: CommandCompleted => throw new RuntimeException("Command execution failed: " + failure)
  }

  def receive = passive
  private def isCloned = Future { new File(workingDirectory, name).exists() }
  private def cloneRepo = runCommand("clone", s"git clone ${repository.url} ${name}", dir = workingDirectory)
  private def pullRepo = runCommand("pull", s"git pull", dir = repoDirectory)
  private def checkout = runCommand("checkout", s"git checkout ${repository.branch}", dir = repoDirectory)
  private def getRevision = runCommand("get-revision", s"git log -n 1", dir = repoDirectory, collectOutput = true)
  private def runCommand(id: String, command: String, dir: File = workingDirectory, collectOutput: Boolean = false) {
    val runner = context.actorOf(Props[CommandRunner], id)
    runner ! Run(id, command, dir, collectOutput = collectOutput)
  }
  private def repoDirectory = new File(workingDirectory, name)
}

object GitWorkingCopy {
  object Activate
  private object Clone
  private object CloneComplete
  private object CheckoutComplete
  private object Pull
  private case class Revision(revision: String)
}
