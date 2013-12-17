package eu.semantiq.easyrider

import java.io.File
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._
import akka.actor.{ActorRef, Props, ActorLogging, Actor}
import akka.event.LoggingReceive
import akka.pattern.PipeToSupport

class GitWorkingCopy(listener: ActorRef, repoDirectory: File, pullFrequency: FiniteDuration)
  extends Actor with ActorLogging with PipeToSupport {
  import GitWorkingCopy._
  import CommandRunner._

  val pullSchedule = context.system.scheduler.schedule(pullFrequency, pullFrequency, self, Pull)

  var repository: GitRepositoryRef = _

  override def postStop() = pullSchedule.cancel()

  def passive = LoggingReceive {
    case ConfigurationUpdated(newConfig) =>
      repository = newConfig
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
    case Pull =>
      pullRepo
      context.become(updating(revision))
  }

  def updating(revision: String): Receive = {
    case Pull => // ignore, already pulling
    case CommandExitCode("pull", 0, _) => getRevision
    case CommandExitCode("get-revision", 0, Some(newRevision)) => if (newRevision != revision) {
      context.become(active(newRevision))
      listener ! AppSupervisor.WorkingCopyUpdated
    } else {
      context.become(active(revision))
    }
    case failure: CommandCompleted => throw new RuntimeException("Command execution failed: " + failure)
  }

  def receive = passive
  private def isCloned = Future { repoDirectory.exists() }
  private def cloneRepo = runCommand("clone", s"git clone ${repository.url} ${repoDirectory.getName}", dir = repoDirectory.getParentFile)
  private def pullRepo = runCommand("pull", s"git pull", dir = repoDirectory)
  private def checkout = runCommand("checkout", s"git checkout ${repository.branch}", dir = repoDirectory)
  private def getRevision = runCommand("get-revision", s"git log -n 1", dir = repoDirectory, collectOutput = true)
  private def runCommand(id: String, command: String, dir: File, collectOutput: Boolean = false) = {
    val runner = context.actorOf(Props[CommandRunner], id)
    runner ! Run(id, command, dir, collectOutput = collectOutput)
    runner
  }
}

object GitWorkingCopy {
  def apply(listener: ActorRef, workingDirectory: File, pullFrequency: FiniteDuration = 30.seconds) =
    Props(classOf[GitWorkingCopy], listener, workingDirectory, pullFrequency)

  case class ConfigurationUpdated(repo: GitRepositoryRef)
  private object Clone
  private object CloneComplete
  private object CheckoutComplete
  private object Pull
  private case class Revision(revision: String)
}
