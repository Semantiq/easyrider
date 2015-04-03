package easyrider.builtin

import java.net.URL
import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import akka.pattern._
import akka.util.Timeout
import easyrider.Applications.ContainerConfiguration
import easyrider.Commands.Failure
import easyrider.Infrastructure._
import easyrider.RemoteAccess._
import easyrider.Repository.Version
import easyrider._
import easyrider.process.{ParallelProcess, CommandMonitor, SequentialProcess}

import scala.concurrent.Future

class ContainerAgent(val eventBus: ActorRef, easyRiderUrl: URL, sshSession: ActorRef, builtInPackageUpload: () => Props,
                   configuration: ContainerConfiguration) extends Actor with SshNodeDirectoryLayout with ActorLogging with EventPublisher {
  implicit val timeout = Timeout(5, TimeUnit.MINUTES)
  implicit val dispatcher = context.system.dispatcher

  sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, "cat " + containerDir(configuration.id) + "/running.version") onSuccess {
    case RunRemoteCommandSuccess(_, Some(output), _, _) =>
      val version = output.trim()
      if (version != "") {
        val state = ContainerRunning(Version(configuration.id.stageId.applicationId, version))
        val snapshotUpdate = SnapshotUpdateDetails[ContainerState](SnapshotEntryType(classOf[ContainerState]), configuration.id.eventKey, Some(state))
        eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), configuration.id.eventKey, Seq()), configuration.id,
          state, snapshotUpdate)
      }
  }

  def configured = LoggingReceive {
    case command@CreateContainer(commandDetails, _, containerId) =>
      def eventDetails = EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId))
      SequentialProcess(context)(
        CommandMonitor(context)(sshSession, RunRemoteCommand(CommandDetails(), configuration.nodeId, "mkdir -p " + versionsDir(containerId))),
        CommandMonitor(context)(sshSession, RunRemoteCommand(CommandDetails(), configuration.nodeId, "mkdir -p " + logDir(containerId))),
        CommandMonitor(context)(sshSession, RunRemoteCommand(CommandDetails(), configuration.nodeId, "mkdir -p " + etcDir(containerId))),
        CommandMonitor(context)(sshSession, RunRemoteCommand(CommandDetails(), configuration.nodeId, "mkdir -p " + dataDir(containerId))))
      .onSuccess(_ => eventBus ! ContainerStateChangedEvent(eventDetails, containerId, ContainerCreated, snapshotUpdate(ContainerCreated)))
      .onFailure(failure => eventBus ! ContainerStateChangedEvent(eventDetails, containerId, ContainerCreationFailed, snapshotUpdate(ContainerCreationFailed)))
      .run()
    case DeployVersion(commandDetails, containerId, version) =>
      val eventKey = containerId.eventKey append version.number
      val packageFile = version.number + ".tar.gz"
      val packageFolder = versionsDir(containerId)
      val originalSender = sender()
      eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId)), version, DeploymentInProgress, snapshotUpdate(eventKey, version, DeploymentInProgress))
      val upload = context.actorOf(builtInPackageUpload())
      upload ? BuiltInPackageUpload.Upload(CommandDetails(), version, configuration.nodeId, packageFolder, packageFile) flatMap {
        case _: BuiltInPackageUpload.UploadComplete => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"rm -r $packageFolder/${version.number}")
        case failure: Failure => Future(failure)
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"mkdir -p $packageFolder/${version.number}")
        case failure: Failure => Future(failure)
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"ln -s `pwd`/${etcDir(containerId)} $packageFolder/${version.number}/etc")
        case failure: Failure => Future(failure)
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"ln -s `pwd`/${logDir(containerId)} $packageFolder/${version.number}/log")
        case failure: Failure => Future(failure)
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"ln -s `pwd`/${dataDir(containerId)} $packageFolder/${version.number}/data")
        case failure: Failure => Future(failure)
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"tar -zxf $packageFolder/$packageFile -C $packageFolder/${version.number}")
        case failure: Failure => Future(failure)
      } onSuccess {
        case _: RunRemoteCommandSuccess => publishEvent(
          VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId)), version, DeploymentCompleted, snapshotUpdate(eventKey, version, DeploymentCompleted)),
          originalSender)
        case failure: Failure =>
          val deploymentFailed = DeploymentFailed(failure.failureMessage)
          publishEvent(
          VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId)), version, deploymentFailed, snapshotUpdate(eventKey, version, deploymentFailed)),
          originalSender)
      }
    case StartContainer(commandDetails, containerId, version) =>
      val script = "./bin/run" // TODO: evaluate from config
      val args = "" // TODO: evaluate from config
      ParallelProcess(context)(successes => successes.head,
        CommandMonitor(context)(sshSession, RunRemoteCommand(CommandDetails(), configuration.nodeId, s"(cd ${versionsDir(containerId)}/${version.number}; $script $args > /dev/null 2> /dev/null &\necho $$! > ./running.pid)")),
        CommandMonitor(context)(sshSession, RunRemoteCommand(CommandDetails(), configuration.nodeId, "echo '" + version.number + "' > " + containerDir(containerId) + "/running.version")))
      .onSuccess(_ => eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId)), containerId, ContainerRunning(version), snapshotUpdate(ContainerRunning(version))))
      .onFailure(failure => sender() ! failure)
      .run()
    case StopContainer(commandDetails, containerId, immediate) =>
      sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, "cat " + containerDir(containerId) + "/running.version") flatMap {
        case RunRemoteCommandSuccess(_, Some(output), _, _) =>
          val runningVersionNumber = output.trim()
          // TODO: this event needs to be sent immediately without waiting for ssh session
          val stopping = ContainerStopping(Version(containerId.stageId.applicationId, runningVersionNumber.trim))
          eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId)), containerId, stopping, snapshotUpdate(stopping))
          sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, "(cd ./" + versionsDir(containerId) + "/" + runningVersionNumber + "/; kill -15 `cat running.pid`; rm running.pid)")
      } onSuccess {
        case _: RunRemoteCommandSuccess =>
          eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId)), containerId, ContainerCreated, snapshotUpdate(ContainerCreated))
      }
    case RemoveContainer(commandDetails, containerId, force) =>
      if (!force) {
        // TODO: check that the container is stopped before continuing
      }
      sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"rm -rf ${containerDir(containerId)}") onSuccess {
        case _: RunRemoteCommandSuccess =>
          eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId), removal = true), containerId, ContainerRemoved, snapshotUpdate(ContainerRemoved))
      }
    case DeployConfigurationFile(commandDetails, containerId, path, fileName, content) =>
      log.info("Deploying configuration {}: {}/{}", containerId, path, fileName)
      sshSession ? UpdateFile(CommandDetails(), configuration.nodeId, path, fileName, content) onSuccess {
        case _: UpdateFileSuccess => eventBus ! DeployConfigurationFileComplete(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId)), containerId)
      }
    case UnDeployVersion(commandDetails, containerId, version) =>
      val eventKey = containerId.eventKey append version.number
      eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId)), version, UnDeploymentInProgress, snapshotUpdate(eventKey, version, UnDeploymentInProgress))
      sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"rm -r ${versionsDir(containerId)}/${version.number}") flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"rm ${versionsDir(containerId)}/${version.number}.tar.bz2")
        case failure: Failure => Future(failure)
      } onSuccess {
        case _: RunRemoteCommandSuccess => eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId), removal = true), version, UnDeployed, snapshotUpdate(eventKey, version, UnDeployed))
        case failure: Failure =>
          val failed = DeploymentFailed(failure.failureMessage)
          eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId)), version, failed, snapshotUpdate(eventKey, version, failed))
      }
  }

  override def receive = configured

  private def snapshotUpdate(state: ContainerState) = SnapshotUpdateDetails(SnapshotEntryType(classOf[ContainerState]), configuration.id.eventKey, Some(state))
  private def snapshotUpdate(eventKey: EventKey, version: Version, state: DeploymentState) = SnapshotUpdateDetails(SnapshotEntryType(classOf[DeploymentInfo]), eventKey, Some(DeploymentInfo(version, state)))

}

object ContainerAgent {
  def apply(eventBus: ActorRef, easyRiderUrl: URL, sshSession: ActorRef, builtInPackageUpload: () => Props)(configuration: ContainerConfiguration) = Props(classOf[ContainerAgent], eventBus, easyRiderUrl, sshSession, builtInPackageUpload, configuration)
}
