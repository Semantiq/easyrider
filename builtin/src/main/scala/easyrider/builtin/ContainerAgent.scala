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

class ContainerAgent(val eventBus: ActorRef, easyRiderUrl: URL, sshSession: ActorRef, builtInPackageUpload: () => Props,
                   configuration: ContainerConfiguration) extends Actor with SshNodeDirectoryLayout with ActorLogging with EventPublisher {
  implicit val timeout = Timeout(5, TimeUnit.MINUTES)
  implicit val dispatcher = context.system.dispatcher

  sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, "cat " + containerDir(configuration.id) + "/running.version") onSuccess {
    case RunRemoteCommandSuccess(_, Some(output), _, _) =>
      val version = output.trim()
      if (version != "") {
        eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), configuration.id.eventKey, Seq()), configuration.id,
          ContainerRunning(Version(configuration.id.stageId.applicationId, version)))
      }
  }

  def configured = LoggingReceive {
    case CreateContainer(commandDetails, _, containerId) =>
      def eventDetails = EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId))
      sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, "mkdir -p " + versionsDir(containerId)) flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, "mkdir -p " + logDir(containerId))
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, "mkdir -p " + etcDir(containerId))
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, "mkdir -p " + dataDir(containerId))
      } onSuccess {
        case _: RunRemoteCommandSuccess => eventBus ! ContainerStateChangedEvent(eventDetails, containerId, ContainerCreated)
        case Failure(_, message, _, _) => eventBus ! ContainerStateChangedEvent(eventDetails, containerId, ContainerCreationFailed)
      }
    case DeployVersion(commandDetails, containerId, version) =>
      val eventKey = containerId.eventKey append version.number
      val packageFile = version.number + ".tar.gz"
      val packageFolder = versionsDir(containerId)
      eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId)), version, DeploymentInProgress)
      val upload = context.actorOf(builtInPackageUpload())
      upload ? BuiltInPackageUpload.Upload(version, configuration.nodeId, packageFolder, packageFile) flatMap {
        case _: BuiltInPackageUpload.UploadComplete => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"rm -r $packageFolder/${version.number}")
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"mkdir -p $packageFolder/${version.number}")
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"ln -s `pwd`/${etcDir(containerId)} $packageFolder/${version.number}/etc")
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"ln -s `pwd`/${logDir(containerId)} $packageFolder/${version.number}/log")
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"ln -s `pwd`/${dataDir(containerId)} $packageFolder/${version.number}/data")
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"tar -zxf $packageFolder/$packageFile -C $packageFolder/${version.number}")
      } onSuccess {
        case _: RunRemoteCommandSuccess => publishEvent(VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId)), version, DeploymentCompleted))
      }
    case StartContainer(commandDetails, containerId, version) =>
      val script = "./bin/run" // TODO: evaluate from config
      val args = "" // TODO: evaluate from config
      val startInitFuture = sshSession ? RunRemoteCommand(CommandDetails(CommandId.generate(), TraceMode()), configuration.nodeId, s"(cd ${versionsDir(containerId)}/${version.number}; $script $args > /dev/null 2> /dev/null &\necho $$! > ./running.pid)")
      val saveVersionFuture = sshSession ? RunRemoteCommand(CommandDetails(CommandId.generate(), TraceMode()), configuration.nodeId, "echo '" + version.number + "' > " + containerDir(containerId) + "/running.version")
      for {
        startInit <- startInitFuture
        saveVersion <- saveVersionFuture
      } yield {
        eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId)), containerId, ContainerRunning(version))
      }
    case StopContainer(commandDetails, containerId, immediate) =>
      sshSession ? RunRemoteCommand(CommandDetails(CommandId.generate(), TraceMode()), configuration.nodeId, "cat " + containerDir(containerId) + "/running.version") flatMap {
        case RunRemoteCommandSuccess(_, Some(output), _, _) =>
          val runningVersionNumber = output.trim()
          // TODO: this event needs to be sent immediately without waiting for ssh session
          eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId)), containerId, ContainerStopping(Version(containerId.stageId.applicationId, runningVersionNumber.trim)))
          sshSession ? RunRemoteCommand(CommandDetails(CommandId.generate(), TraceMode()), configuration.nodeId, "(cd ./" + versionsDir(containerId) + "/" + runningVersionNumber + "/; kill -15 `cat running.pid`; rm running.pid)")
      } onSuccess {
        case _: RunRemoteCommandSuccess =>
          eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId)), containerId, ContainerCreated)
      }
    case RemoveContainer(commandDetails, containerId, force) =>
      if (!force) {
        // TODO: check that the container is stopped before continuing
      }
      sshSession ? RunRemoteCommand(CommandDetails(CommandId.generate(), TraceMode()), configuration.nodeId, s"rm -rf ${containerDir(containerId)}") onSuccess {
        case _: RunRemoteCommandSuccess =>
          eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId), removal = true), containerId, ContainerRemoved)
      }
    case DeployConfigurationFile(commandDetails, containerId, path, fileName, content) =>
      log.info("Deploying configuration {}: {}/{}", containerId, path, fileName)
      sshSession ? UpdateFile(CommandDetails(CommandId.generate(), TraceMode()), configuration.nodeId, path, fileName, content) onSuccess {
        case _: UpdateFileSuccess => eventBus ! DeployConfigurationFileComplete(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId)), containerId)
      }
    case UnDeployVersion(commandDetails, containerId, version) =>
      val eventKey = containerId.eventKey append version.number
      eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId)), version, UnDeploymentInProgress)
      sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"rm -r ${versionsDir(containerId)}/${version.number}") flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.nodeId, s"rm ${versionsDir(containerId)}/${version.number}.tar.bz2")
      } onSuccess {
        case _: RunRemoteCommandSuccess => eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId), removal = true), version, UnDeployed)
      }
  }

  override def receive = configured
}

object ContainerAgent {
  def apply(eventBus: ActorRef, easyRiderUrl: URL, sshSession: ActorRef, builtInPackageUpload: () => Props)(configuration: ContainerConfiguration) = Props(classOf[ContainerAgent], eventBus, easyRiderUrl, sshSession, builtInPackageUpload, configuration)
}
