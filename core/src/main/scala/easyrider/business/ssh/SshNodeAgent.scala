package easyrider.business.ssh

import java.net.URL
import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import akka.pattern._
import akka.util.Timeout
import easyrider.Applications.ContainerId
import easyrider.Commands.Failure
import easyrider.Infrastructure._
import easyrider.Repository.Version
import easyrider._
import easyrider.RemoteAccess._
import easyrider.business.ssh.SshInfrastructure.{NodeConfiguration, NodeConfigurationUpdatedEvent, CreateNode}

class SshNodeAgent(eventBus: ActorRef, easyRiderUrl: URL, sshSession: ActorRef, builtInPackageUpload: () => Props) extends Actor with SshNodeDirectoryLayout with ActorLogging {
  implicit val timeout = Timeout(5, TimeUnit.MINUTES)
  implicit val dispatcher = context.system.dispatcher
  val containers = Set[ContainerId]()

  def unConfigured = LoggingReceive {
    case CreateNode(commandDetails, configuration) =>
      eventBus ! NodeUpdatedEvent(EventDetails(EventId.generate(), EventKey(configuration.id.id), Seq()), configuration.id, CreatingNode)
      eventBus ! NodeConfigurationUpdatedEvent(EventDetails(EventId.generate(), EventKey(configuration.id.id), Seq(commandDetails.commandId)), configuration)
      sshSession ! RunRemoteCommand(CommandDetails(), configuration.id, "mkdir -p easyrider")
      eventBus ! NodeUpdatedEvent(EventDetails(EventId.generate(), EventKey(configuration.id.id), Seq()), configuration.id, NodeCreated)
      context.become(configured(configuration, sshSession))
  }

  def configured(configuration: NodeConfiguration, sshSession: ActorRef) = LoggingReceive {
    case CreateContainer(commandDetails, _, containerId) =>
      def eventDetails = EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId))
      sshSession ? RunRemoteCommand(CommandDetails(), configuration.id, "mkdir -p " + versionsDir(containerId)) flatMap {
        case RunRemoteCommandSuccess(_, _) => sshSession ? RunRemoteCommand(CommandDetails(), configuration.id, "mkdir -p " + logDir(containerId))
      } flatMap {
        case RunRemoteCommandSuccess(_, _) => sshSession ? RunRemoteCommand(CommandDetails(), configuration.id, "mkdir -p " + etcDir(containerId))
      } flatMap {
        case RunRemoteCommandSuccess(_, _) => sshSession ? RunRemoteCommand(CommandDetails(), configuration.id, "mkdir -p " + dataDir(containerId))
      } onSuccess {
        case RunRemoteCommandSuccess(_, _) => eventBus ! ContainerStateChangedEvent(eventDetails, containerId, ContainerCreated)
        case Failure(_, message, _) => eventBus ! ContainerStateChangedEvent(eventDetails, containerId, ContainerCreationFailed)
      }
    case DeployVersion(commandDetails, containerId, version) =>
      val eventKey = containerId.eventKey append version.number
      val packageFile = version.number + ".tar.bz2"
      val packageFolder = versionsDir(containerId)
      eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId)), version, DeploymentInProgress)
      val upload = context.actorOf(builtInPackageUpload())
      upload ? BuiltInPackageUpload.Upload(version, configuration.id, packageFolder, packageFile) flatMap {
        case _: BuiltInPackageUpload.UploadComplete => sshSession ? RunRemoteCommand(CommandDetails(), configuration.id, s"rm -r $packageFolder/${version.number}")
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.id, s"mkdir -p $packageFolder/${version.number}")
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.id, s"ln -s `pwd`/${etcDir(containerId)} $packageFolder/${version.number}/etc")
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.id, s"ln -s `pwd`/${logDir(containerId)} $packageFolder/${version.number}/log")
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.id, s"ln -s `pwd`/${dataDir(containerId)} $packageFolder/${version.number}/data")
      } flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.id, s"tar -jxf $packageFolder/$packageFile -C $packageFolder/${version.number}")
      } onSuccess {
        case _: RunRemoteCommandSuccess => eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId)), version, DeploymentCompleted)
      }
    case StartContainer(commandDetails, containerId, version) =>
      // TODO: create real token
      val authentication = """{"jsonClass":"easyrider.Api$AuthenticateUser"}"""
      val startInitFuture = sshSession ? RunRemoteCommand(CommandDetails(CommandId.generate(), TraceMode()), configuration.id, s"( cd ${versionsDir(containerId)}/${version.number}/; ./init start $easyRiderUrl $authentication )")
      val saveVersionFuture = sshSession ? RunRemoteCommand(CommandDetails(CommandId.generate(), TraceMode()), configuration.id, "echo '" + version.number + "' > " + containerDir(containerId) + "/running.version")
      for {
        startInit <- startInitFuture
        saveVersion <- saveVersionFuture
      } yield {
        eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId)), containerId, ContainerRunning(version))
      }
    case StopContainer(commandDetails, containerId, immediate) =>
      sshSession ? RunRemoteCommand(CommandDetails(CommandId.generate(), TraceMode()), configuration.id, "cat " + containerDir(containerId) + "/running.version") flatMap {
        case RunRemoteCommandSuccess(_, Some(output)) =>
          val runningVersionNumber = output.trim()
          // TODO: this event needs to be sent immediately without waiting for ssh session
          eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId)), containerId, ContainerStopping(Version(containerId.stageId.applicationId, runningVersionNumber.trim)))
          sshSession ? RunRemoteCommand(CommandDetails(CommandId.generate(), TraceMode()), configuration.id, "./" + versionsDir(containerId) + "/" + runningVersionNumber + "/init stop")
      } onSuccess {
        case RunRemoteCommandSuccess(_, _) =>
          eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId)), containerId, ContainerCreated)
      }
    case RemoveContainer(commandDetails, containerId, force) =>
      if (!force) {
        // TODO: check that the container is stopped before continuing
      }
      sshSession ? RunRemoteCommand(CommandDetails(CommandId.generate(), TraceMode()), configuration.id, s"rm -rf ${containerDir(containerId)}") onSuccess {
        case _: RunRemoteCommandSuccess =>
          eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId), removal = true), containerId, ContainerRemoved)
      }
    case DeployConfigurationFile(commandDetails, containerId, path, fileName, content) =>
      log.info("Deploying configuration {}: {}/{}", containerId, path, fileName)
      sshSession ? UpdateFile(CommandDetails(CommandId.generate(), TraceMode()), configuration.id, path, fileName, content) onSuccess {
        case _: UpdateFileSuccess => eventBus ! DeployConfigurationFileComplete(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId)), containerId)
      }
    case UnDeployVersion(commandDetails, containerId, version) =>
      val eventKey = containerId.eventKey append version.number
      eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId)), version, UnDeploymentInProgress)
      sshSession ? RunRemoteCommand(CommandDetails(), configuration.id, s"rm -r ${versionsDir(containerId)}/${version.number}") flatMap {
        case _: RunRemoteCommandSuccess => sshSession ? RunRemoteCommand(CommandDetails(), configuration.id, s"rm ${versionsDir(containerId)}/${version.number}.tar.bz2")
      } onSuccess {
        case RunRemoteCommandSuccess(_, _) => eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId), removal = true), version, UnDeployed)
      }
  }

  override def receive = unConfigured
}

object SshNodeAgent {
  def apply(eventBus: ActorRef, easyRiderUrl: URL, sshSession: ActorRef, builtInPackageUpload: () => Props)() = Props(classOf[SshNodeAgent], eventBus, easyRiderUrl, sshSession, builtInPackageUpload)
}
