package eu.semantiq.easyrider.builder

import akka.actor._
import eu.semantiq.easyrider.AppRepository
import java.io.File
import scala.concurrent.duration._
import eu.semantiq.easyrider.AppRepository.PackageRef
import scala.concurrent.ExecutionContext.Implicits.global
import org.json4s._
import org.json4s.jackson.JsonMethods._
import eu.semantiq.easyrider.PackageMetadata
import eu.semantiq.easyrider.GitRepositoryRef
import scala.util.{Failure, Try}
import akka.event.LoggingReceive

class AppBuilder(app: String, appRepo: ActorRef, workingDirectory: File, gitPollingInterval: FiniteDuration,
                  compilationTimeout: FiniteDuration) extends Actor with Stash with ActorLogging {
  import AppBuilder._
  private implicit val formats = DefaultFormats
  private val git = context.actorOf(GitWorkingCopy(self, workingCopyLocation, gitPollingInterval), "working-copy")
  private val compiler = context.actorOf(Compiler(self, workingCopyLocation, compilationTimeout), "compiler")
  context.system.scheduler.schedule(gitPollingInterval, gitPollingInterval, self, Pull)

  override def preStart() {
    workingDirectory.mkdir()
  }

  // TODO: ask for configuration, as there are no startup guarantee that we'll get it after restart
  override def postRestart(reason: Throwable): Unit = {
    log.error("AppBuilder got restarted, currently it will not be able to recover and restart is required", reason)
    super.postRestart(reason)
  }

  def created: Receive = {
    case ConfigurationUpdated(gitConfig) =>
      git ! GitWorkingCopy.ConfigurationUpdated(gitConfig)
      context.become(awaitingCommits(None))
      appRepo ! AppRepository.GetVersionAvailable(app)
    case Pull => // ignore
  }

  def awaitingCommits(currentVersion: Option[String]): Receive = LoggingReceive {
    case AppRepository.VersionAvailable(newApp, newVersion) =>
      context.become(awaitingCommits(Some(newVersion)))
    case GitWorkingCopy.WorkingCopyUpdated(version) =>
      if (currentVersion.exists(_ == version)) {
        log.info("Working copy version is already in repository: {}", version)
      } else if (!compilationSettingsLocation.exists()) {
        log.info("Working copy doesn't have .easyrider.json - waiting for updates")
      } else if (compilationSettings.isFailure) {
        log.info("Working copy contains invalid .easyrider.json: {}", compilationSettings.asInstanceOf[Failure[Any]].exception.toString)
      } else {
        compiler ! Compiler.Compile(compilationSettings.get.compilation.command)
        context.become(compiling(version))
      }
    case ConfigurationUpdated(gitConfig) => git ! GitWorkingCopy.ConfigurationUpdated(gitConfig)
    case Pull => git ! GitWorkingCopy.Pull
  }

  def compiling(version: String): Receive = LoggingReceive {
    case Compiler.CompilationSuccessful =>
      log.info("Deploying application {} in version {} to package repository", app, version)
      val packageRef = PackageRef.fromFolder(new File(workingCopyLocation, compilationSettings.get.compilation.distributionFolder))
      appRepo ! AppRepository.DeployVersion(app, version, packageRef, compilationSettings.get)
      context.become(awaitingCommits(Some(version)))
      unstashAll()
    case Compiler.CompilationFailure =>
      context.become(awaitingCommits(Some(version)))
      unstashAll()
    case Pull => // ignore
    case _: ConfigurationUpdated => stash()
    case _: GitWorkingCopy.WorkingCopyUpdated => stash()
  }

  def receive = created

  private def workingCopyLocation = new File(workingDirectory, "working-copy")
  private def compilationSettingsLocation = new File(workingCopyLocation, ".easyrider.json")
  private def compilationSettings = Try(parse(compilationSettingsLocation).extract[PackageMetadata])
}

object AppBuilder {
  def apply(app: String, appRepo: ActorRef, workingDirectory: File, gitPollingInterval: FiniteDuration = 30.seconds,
            compilationTimeout: FiniteDuration = 15.minutes) =
    Props(classOf[AppBuilder], app, appRepo, workingDirectory, gitPollingInterval, compilationTimeout)

  case class ConfigurationUpdated(git: GitRepositoryRef)
  object Pull
}
