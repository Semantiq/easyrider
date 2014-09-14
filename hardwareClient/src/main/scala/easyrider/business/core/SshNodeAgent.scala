package easyrider.business.core

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import akka.pattern._
import akka.util.Timeout
import easyrider.Applications.ContainerId
import easyrider.Infrastructure._
import easyrider.Repository.Version
import easyrider.SshInfrastructure._
import easyrider._

class SshNodeAgent(eventBus: ActorRef, sshSessionFactory: (NodeConfiguration) => Props) extends Actor with SshNodeDirectoryLayout with ActorLogging {
  implicit val timeout = Timeout(60, TimeUnit.SECONDS)
  implicit val dispatcher = context.system.dispatcher
  val containers = Set[ContainerId]()

  def unConfigured = LoggingReceive {
    case CreateNode(commandId, configuration) =>
      eventBus ! NodeUpdatedEvent(EventDetails(EventId.generate(), EventKey(configuration.id.id), Seq()), configuration.id, CreatingNode)
      eventBus ! NodeConfigurationUpdatedEvent(EventDetails(EventId.generate(), EventKey(configuration.id.id), Seq(commandId)), configuration)
      val sshSession = context.actorOf(sshSessionFactory(configuration))
      sshSession ! RunSshCommand(CommandId.generate(), configuration.id, "mkdir -p easyrider")
      eventBus ! NodeUpdatedEvent(EventDetails(EventId.generate(), EventKey(configuration.id.id), Seq()), configuration.id, NodeCreated)
      context.become(configured(configuration, sshSession))
  }

  def configured(configuration: NodeConfiguration, sshSession: ActorRef) = LoggingReceive {
    case CreateContainer(commandId, _, containerId) =>
      def eventDetails = EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId))
      sshSession ? RunSshCommand(CommandId.generate(), configuration.id, "mkdir -p " + versionsDir(containerId)) onSuccess {
        case RunSshCommandSuccess(_, _) => eventBus ! ContainerStateChangedEvent(eventDetails, ContainerCreated)
        case Failure(_, message, _) => eventBus ! ContainerStateChangedEvent(eventDetails, ContainerCreationFailed)
      }
    case command: DeployVersion =>
      sshSession ! command
    case StartContainer(commandId, containerId, version) =>
      val startInitFuture = sshSession ? RunSshCommand(CommandId.generate(), configuration.id, "./" + versionsDir(containerId) + "/" + version.number + "/init start")
      val saveVersionFuture = sshSession ? RunSshCommand(CommandId.generate(), configuration.id, "echo '" + version.number + "' > " + containerDir(containerId) + "/running.version")
      for {
        startInit <- startInitFuture
        saveVersion <- saveVersionFuture
      } yield {
        eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId)), ContainerRunning(version))
      }
    case StopContainer(commandId, containerId, immediate) =>
      sshSession ? RunSshCommand(CommandId.generate(), configuration.id, "cat " + containerDir(containerId) + "/running.version") flatMap {
        case RunSshCommandSuccess(_, Some(runningVersionNumber)) =>
          eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId)), ContainerStopping(Version(containerId.stageId.applicationId, runningVersionNumber.trim)))
          sshSession ? RunSshCommand(CommandId.generate(), configuration.id, "./" + versionsDir(containerId) + "/" + runningVersionNumber + "/init stop")
      } onSuccess {
        case RunSshCommandSuccess(_, _) =>
          eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId)), ContainerCreated)
      }
  }

  override def receive = unConfigured
}

object SshNodeAgent {
  def apply(eventBus: ActorRef, sshSessionFactory: (NodeConfiguration) => Props)() = Props(classOf[SshNodeAgent], eventBus, sshSessionFactory)
}
