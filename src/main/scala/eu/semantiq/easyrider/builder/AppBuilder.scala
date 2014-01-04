package eu.semantiq.easyrider.builder

import akka.actor.{Stash, Props, Actor, ActorRef}
import eu.semantiq.easyrider.{PackageMetadata, AppRepository, GitRepositoryRef}
import java.io.File
import scala.concurrent.duration._
import eu.semantiq.easyrider.AppRepository.PackageRef
import scala.concurrent.ExecutionContext.Implicits.global
import org.json4s._
import org.json4s.jackson.JsonMethods._

class AppBuilder(app: String, appRepo: ActorRef, workingDirectory: File, gitPollingInterval: FiniteDuration,
                  compilationTimeout: FiniteDuration) extends Actor with Stash {
  import AppBuilder._
  private implicit val formats = DefaultFormats
  private val git = context.actorOf(GitWorkingCopy(self, workingCopyLocation, gitPollingInterval), "working-copy")
  private val compiler = context.actorOf(Compiler(self, workingCopyLocation, compilationTimeout), "compiler")
  context.system.scheduler.schedule(gitPollingInterval, gitPollingInterval, self, Pull)

  override def preStart() {
    workingDirectory.mkdir()
  }

  def created: Receive = {
    case ConfigurationUpdated(gitConfig) =>
      git ! GitWorkingCopy.ConfigurationUpdated(gitConfig)
      context.become(awaitingCommits)
    case Pull => // ignore
  }

  def awaitingCommits: Receive = {
    case GitWorkingCopy.WorkingCopyUpdated(version) =>
      compiler ! Compiler.Compile(compilationSettings.compilation.command)
      context.become(compiling(version))
    case ConfigurationUpdated(gitConfig) => git ! GitWorkingCopy.ConfigurationUpdated(gitConfig)
    case Pull => git ! GitWorkingCopy.Pull
  }

  def compiling(version: String): Receive = {
    case Compiler.CompilationSuccessful =>
      val packageRef = PackageRef.fromFolder(new File(workingCopyLocation, compilationSettings.compilation.distributionFolder))
      appRepo ! AppRepository.DeployVersion(app, version, packageRef, compilationSettings)
      context.become(awaitingCommits)
      unstashAll()
    case Compiler.CompilationFailure =>
      context.become(awaitingCommits)
      unstashAll()
    case Pull => // ignore
    case _ => stash()
  }

  def receive = created

  private def workingCopyLocation = new File(workingDirectory, "working-copy")
  private def compilationSettings = parse(new File(workingCopyLocation, ".easyrider.json")).extract[PackageMetadata]
}

object AppBuilder {
  def apply(app: String, appRepo: ActorRef, workingDirectory: File, gitPollingInterval: FiniteDuration = 30.seconds,
            compilationTimeout: FiniteDuration = 5.minutes) =
    Props(classOf[AppBuilder], app, appRepo, workingDirectory, gitPollingInterval, compilationTimeout)

  case class ConfigurationUpdated(git: GitRepositoryRef)
  object Pull
}
