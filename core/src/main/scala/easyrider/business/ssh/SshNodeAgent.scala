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
import easyrider.business.ssh.SshInfrastructure._

class SshNodeAgent(eventBus: ActorRef, easyRiderUrl: URL, sshSessionFactory: (NodeConfiguration) => Props) extends Actor with SshNodeDirectoryLayout with ActorLogging {
  implicit val timeout = Timeout(5, TimeUnit.MINUTES)
  implicit val dispatcher = context.system.dispatcher
  val containers = Set[ContainerId]()

  def unConfigured = LoggingReceive {
    case CreateNode(commandDetails, configuration) =>
      eventBus ! NodeUpdatedEvent(EventDetails(EventId.generate(), EventKey(configuration.id.id), Seq()), configuration.id, CreatingNode)
      eventBus ! NodeConfigurationUpdatedEvent(EventDetails(EventId.generate(), EventKey(configuration.id.id), Seq(commandDetails.commandId)), configuration)
      val sshSession = context.actorOf(sshSessionFactory(configuration), "sshSession")
      sshSession ! RunSshCommand(CommandDetails(), configuration.id, "mkdir -p easyrider")
      eventBus ! NodeUpdatedEvent(EventDetails(EventId.generate(), EventKey(configuration.id.id), Seq()), configuration.id, NodeCreated)
      context.become(configured(configuration, sshSession))
  }

  def configured(configuration: NodeConfiguration, sshSession: ActorRef) = LoggingReceive {
    case CreateContainer(commandDetails, _, containerId) =>
      def eventDetails = EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId))
      sshSession ? RunSshCommand(CommandDetails(), configuration.id, "mkdir -p " + versionsDir(containerId)) flatMap {
        case RunSshCommandSuccess(_, _) => sshSession ? RunSshCommand(CommandDetails(), configuration.id, "mkdir -p " + logDir(containerId))
      } flatMap {
        case RunSshCommandSuccess(_, _) => sshSession ? RunSshCommand(CommandDetails(), configuration.id, "mkdir -p " + etcDir(containerId))
      } flatMap {
        case RunSshCommandSuccess(_, _) => sshSession ? RunSshCommand(CommandDetails(), configuration.id, "mkdir -p " + dataDir(containerId))
      } onSuccess {
        case RunSshCommandSuccess(_, _) => eventBus ! ContainerStateChangedEvent(eventDetails, ContainerCreated)
        case Failure(_, message, _) => eventBus ! ContainerStateChangedEvent(eventDetails, ContainerCreationFailed)
      }
    case DeployVersion(commandDetails, containerId, version) =>
      val eventKey = containerId.eventKey append version.number
      val packageFile = version.number + ".tar.bz2"
      val packageFolder = versionsDir(containerId)
      eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId)), version, DeploymentInProgress)
      sshSession ? SftpUploadCommand(CommandDetails(), version, packageFolder, packageFile) flatMap {
        case _: SftpUploadCommandSuccess => sshSession ? RunSshCommand(CommandDetails(), configuration.id, s"rm -r $packageFolder/${version.number}")
      } flatMap {
        case _: RunSshCommandSuccess => sshSession ? RunSshCommand(CommandDetails(), configuration.id, s"mkdir -p $packageFolder/${version.number}")
      } flatMap {
        case _: RunSshCommandSuccess => sshSession ? RunSshCommand(CommandDetails(), configuration.id, s"ln -s `pwd`/${etcDir(containerId)} $packageFolder/${version.number}/etc")
      } flatMap {
        case _: RunSshCommandSuccess => sshSession ? RunSshCommand(CommandDetails(), configuration.id, s"ln -s `pwd`/${logDir(containerId)} $packageFolder/${version.number}/log")
      } flatMap {
        case _: RunSshCommandSuccess => sshSession ? RunSshCommand(CommandDetails(), configuration.id, s"ln -s `pwd`/${dataDir(containerId)} $packageFolder/${version.number}/data")
      } flatMap {
        case _: RunSshCommandSuccess => sshSession ? RunSshCommand(CommandDetails(), configuration.id, s"tar -jxf $packageFolder/$packageFile -C $packageFolder/${version.number}")
      } onSuccess {
        case _: RunSshCommandSuccess => eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId)), version, DeploymentCompleted)
      }
    case StartContainer(commandDetails, containerId, version) =>
      // TODO: create real token
      val authentication = """{"jsonClass":"easyrider.Api$AuthenticateUser"}"""
      val startInitFuture = sshSession ? RunSshCommand(CommandDetails(CommandId.generate(), TraceMode()), configuration.id, s"( cd ${versionsDir(containerId)}/${version.number}/; ./init start $easyRiderUrl $authentication )")
      val saveVersionFuture = sshSession ? RunSshCommand(CommandDetails(CommandId.generate(), TraceMode()), configuration.id, "echo '" + version.number + "' > " + containerDir(containerId) + "/running.version")
      for {
        startInit <- startInitFuture
        saveVersion <- saveVersionFuture
      } yield {
        eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId)), ContainerRunning(version))
      }
    case StopContainer(commandDetails, containerId, immediate) =>
      sshSession ? RunSshCommand(CommandDetails(CommandId.generate(), TraceMode()), configuration.id, "cat " + containerDir(containerId) + "/running.version") flatMap {
        case RunSshCommandSuccess(_, Some(output)) =>
          val runningVersionNumber = output.trim()
          // TODO: this event needs to be sent immediately without waiting for ssh session
          eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId)), ContainerStopping(Version(containerId.stageId.applicationId, runningVersionNumber.trim)))
          sshSession ? RunSshCommand(CommandDetails(CommandId.generate(), TraceMode()), configuration.id, "./" + versionsDir(containerId) + "/" + runningVersionNumber + "/init stop")
      } onSuccess {
        case RunSshCommandSuccess(_, _) =>
          eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId)), ContainerCreated)
      }
    case DeployConfigurationFile(commandDetails, containerId, path, fileName, content) =>
      log.info("Deploying configuration {}: {}/{}", containerId, path, fileName)
      sshSession ? SftpUpdateFile(CommandDetails(CommandId.generate(), TraceMode()), configuration.id, path, fileName, content) onSuccess {
        case _: SftpUpdateFileSuccess => eventBus ! DeployConfigurationFileComplete(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandDetails.commandId)), containerId)
      }
    case UnDeployVersion(commandDetails, containerId, version) =>
      val eventKey = containerId.eventKey append version.number
      eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId)), version, UnDeploymentInProgress)
      sshSession ? RunSshCommand(CommandDetails(), configuration.id, s"rm -r ${versionsDir(containerId)}/${version.number}") flatMap {
        case _: RunSshCommandSuccess => sshSession ? RunSshCommand(CommandDetails(), configuration.id, s"rm ${versionsDir(containerId)}/${version.number}.tar.bz2")
      } onSuccess {
        case RunSshCommandSuccess(_, _) => eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandDetails.commandId), removal = true), version, UnDeployed)
      }
  }

  override def receive = unConfigured
}

object SshNodeAgent {
  def apply(eventBus: ActorRef, easyRiderUrl: URL, sshSessionFactory: (NodeConfiguration) => Props)() = Props(classOf[SshNodeAgent], eventBus, easyRiderUrl, sshSessionFactory)
}
