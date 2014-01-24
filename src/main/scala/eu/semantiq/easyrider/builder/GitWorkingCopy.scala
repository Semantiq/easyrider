package eu.semantiq.easyrider.builder

import java.io.File
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._
import akka.actor._
import akka.event.LoggingReceive
import akka.pattern.PipeToSupport
import eu.semantiq.easyrider.{GitRepositoryRef, CommandRunner}
import eu.semantiq.easyrider.CommandRunner.{Run, CommandCompleted, CommandExitCode}
import org.apache.commons.io.FileUtils
import eu.semantiq.easyrider.CommandRunner.CommandExitCode
import eu.semantiq.easyrider.GitRepositoryRef
import scala.Some
import eu.semantiq.easyrider.CommandRunner.Run

class GitWorkingCopy(listener: ActorRef, repoDirectory: File, pullFrequency: FiniteDuration)
  extends Actor with ActorLogging with PipeToSupport with Stash {
  import GitWorkingCopy._
  // TODO: implement configuration update

  var repository: GitRepositoryRef = _

  def passive = LoggingReceive {
    case ConfigurationUpdated(newConfig) =>
      repository = newConfig
      isCloned map {
        case true => Checkout
        case false => Clone
      } pipeTo self
      context.become(activating)
  }

  def activating: Receive = LoggingReceive {
    case Clone => cloneRepo
    case CommandExitCode("clone", 0, _) => self ! Checkout
    case Checkout => checkout
    case CommandExitCode("checkout", 0, _) => self ! CheckoutComplete
    case CheckoutComplete => getRevision
    case CommandExitCode("get-revision", 0, Some(revision)) =>
       context.become(active(revision))
       listener ! WorkingCopyUpdated(extractVersion(revision))
    case failure: CommandCompleted => throw new RuntimeException("Command execution failed: " + failure)
  }

  def active(revision: String): Receive = LoggingReceive {
    case Pull =>
      pullRepo
      context.become(updating(revision))
    case message @ ConfigurationUpdated(newRepository) =>
      if (repository.url != newRepository.url) {
        log.info("Git url changed, scheduling fresh clone from {}", newRepository)
        repository = newRepository
        FileUtils.deleteDirectory(repoDirectory)
        context.become(passive)
        self ! message
      } else if (repository.branch != newRepository.branch) {
        log.info("Git branch changed, scheduling checkout of {}", newRepository)
        repository = newRepository
        context.become(activating)
        self ! Checkout
      }
  }

  def updating(revision: String): Receive = LoggingReceive {
    case Pull => // ignore, already pulling
    case CommandExitCode("pull", 0, _) => getRevision
    case CommandExitCode("get-revision", 0, Some(newRevision)) => if (newRevision != revision) {
      context.become(active(newRevision))
      listener ! WorkingCopyUpdated(extractVersion(newRevision))
      unstashAll()
    } else {
      context.become(active(revision))
      unstashAll()
    }
    case failure: CommandCompleted => throw new RuntimeException("Command execution failed: " + failure)
    case _ => stash()
  }

  def receive = passive
  private def isCloned = Future { repoDirectory.exists() }
  private def cloneRepo = runCommand("clone", s"git clone ${repository.url} ${repoDirectory.getName}", dir = repoDirectory.getParentFile)
  private def pullRepo = runCommand("pull", s"git pull", dir = repoDirectory)
  private def checkout = runCommand("checkout", s"git checkout ${repository.branch}", dir = repoDirectory)
  private def getRevision = runCommand("get-revision", s"git log -n 1", dir = repoDirectory, collectOutput = true)
  private def runCommand(id: String, command: String, dir: File, collectOutput: Boolean = false) = {
    val runner = context.actorOf(CommandRunner(), id)
    runner ! Run(id, command, dir, collectOutput = collectOutput)
    runner
  }
  private def extractVersion(revision: String) = "\\b[0-9a-f]{5,40}\\b".r.findFirstIn(revision).get
}

object GitWorkingCopy {
  def apply(listener: ActorRef, workingDirectory: File, pullFrequency: FiniteDuration = 30.seconds) =
    Props(classOf[GitWorkingCopy], listener, workingDirectory, pullFrequency)

  case class ConfigurationUpdated(repo: GitRepositoryRef)
  case class WorkingCopyUpdated(version: String)
  object Pull
  private object Clone
  private object Checkout
  private object CheckoutComplete
  private case class Revision(revision: String)
}
