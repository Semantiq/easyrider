package eu.semantiq.easyrider.builder

import akka.actor.{Props, Actor, ActorRef}
import eu.semantiq.easyrider.{Compilation, GitRepositoryRef}
import eu.semantiq.easyrider.builder.AppBuilder.{Pull, ConfigurationUpdated}
import java.io.File
import scala.concurrent.duration.FiniteDuration

class AppBuilder(appRepo: ActorRef, workingDirectory: File, gitPollingInterval: FiniteDuration) extends Actor {

  context.system.scheduler.schedule(gitPollingInterval, gitPollingInterval, self, Pull)
  val git = context.actorOf(GitWorkingCopy(self, workingCopyLocation, gitPollingInterval), "working-copy")

  def created: Receive = {
    case ConfigurationUpdated(gitConfig, compilerConfig) => ???
    case Pull => // ignore
  }

  def awaitingCommits: Receive = {
    case GitWorkingCopy.WorkingCopyUpdated(version) => ???
    case Pull => git ! GitWorkingCopy.Pull
  }

  def receive = created

  private def workingCopyLocation = new File(workingDirectory, "working-copy")
}

object AppBuilder {
  def apply(appRepo: ActorRef, workingDirectory: File) = Props(classOf[AppBuilder], appRepo)

  case class ConfigurationUpdated(git: GitRepositoryRef, compilation: Compilation)
  object Pull
}
