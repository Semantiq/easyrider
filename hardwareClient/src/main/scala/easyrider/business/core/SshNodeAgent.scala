package easyrider.business.core

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import com.jcraft.jsch.{ChannelExec, JSch}
import easyrider.Applications.ContainerId
import easyrider.Infrastructure._
import easyrider.Repository.Version
import easyrider.SshInfrastructure.{CreateNode, NodeConfiguration, NodeConfigurationUpdatedEvent}
import easyrider.{EventDetails, EventId, EventKey}
import org.apache.commons.io.IOUtils

class SshNodeAgent(eventBus: ActorRef, deployerFactory: (NodeConfiguration, DeployVersion) => Props) extends Actor with SshNodeDirectoryLayout {
  val containers = Set[ContainerId]()

  def unConfigured = LoggingReceive {
    case CreateNode(commandId, configuration) =>
      eventBus ! NodeUpdatedEvent(EventDetails(EventId.generate(), EventKey(configuration.id.id), Seq()), configuration.id, CreatingNode)
      eventBus ! NodeConfigurationUpdatedEvent(EventDetails(EventId.generate(), EventKey(configuration.id.id), Seq(commandId)), configuration)
      runSshCommand(configuration, "mkdir -p easyrider")
      eventBus ! NodeUpdatedEvent(EventDetails(EventId.generate(), EventKey(configuration.id.id), Seq()), configuration.id, NodeCreated)
      context.become(configured(configuration))
  }

  def configured(configuration: NodeConfiguration) = LoggingReceive {
    case CreateContainer(commandId, _, containerId) =>
      def eventDetails = EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId))
      val (exitStatus, output, outputErr) = runSshCommand(configuration, "mkdir -p " + versionsDir(containerId))
      println("out:" + output + " err:" + outputErr)
      exitStatus match {
        case 0 =>
          eventBus ! ContainerStateChangedEvent(eventDetails, ContainerCreated)
        case other =>
          eventBus ! ContainerStateChangedEvent(eventDetails, ContainerCreationFailed)
      }
    case command @ DeployVersion(commandId, containerId, version) =>
      context.actorOf(deployerFactory(configuration, command))
    case StartContainer(commandId, containerId, version) =>
      // TODO: implement
      runSshCommand(configuration, "echo '" + version.number + "' > " + containerDir(containerId) + "/running.version")
      eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId)), ContainerRunning(version))
    case StopContainer(commandId, containerId, immediate) =>
      // TODO: implement
      val (0, runningVersionNumber, _) = runSshCommand(configuration, "cat " + containerDir(containerId) + "/running.version")
      eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId)), ContainerStopping(Version(containerId.stageId.applicationId, runningVersionNumber.trim)))
      Thread.sleep(1000)
      eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId)), ContainerCreated)
  }

  override def receive = unConfigured

  private def runSshCommand(nodeConfiguration: NodeConfiguration, command: String) = {
    val jsch = new JSch()
    val session = jsch.getSession(nodeConfiguration.login, nodeConfiguration.host, nodeConfiguration.port)
    session.setPassword(nodeConfiguration.password)
    session.setConfig("StrictHostKeyChecking", "no")
    session.connect()
    val shell = session.openChannel("exec").asInstanceOf[ChannelExec]
    shell.setCommand(command)
    val in = shell.getInputStream
    val err = shell.getErrStream
    shell.connect()
    val output = IOUtils.toString(in)
    val outputErr = IOUtils.toString(err)
    val exitStatus = shell.getExitStatus
    shell.disconnect()
    session.disconnect()
    (exitStatus, output, outputErr)
  }
}

object SshNodeAgent {
  def apply(eventBus: ActorRef, deployerFactory: (NodeConfiguration, DeployVersion) => Props)() = Props(classOf[SshNodeAgent], eventBus, deployerFactory)
}
