package easyrider.business.core

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import com.jcraft.jsch.{ChannelSftp, ChannelExec, JSch}
import easyrider.Applications.ContainerId
import easyrider.Infrastructure._
import easyrider.SshInfrastructure.{NodeConfigurationUpdatedEvent, CreateNode, NodeConfiguration}
import easyrider.{EventDetails, EventId, EventKey}
import org.apache.commons.io.IOUtils

class SshNodeAgent(eventBus: ActorRef) extends Actor {
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
      val (exitStatus, output) = runSshCommand(configuration, versionsDir(containerId))
      println(output)
      exitStatus match {
        case 0 =>
          eventBus ! ContainerStateChangedEvent(eventDetails, Created)
        case other =>
          eventBus ! ContainerStateChangedEvent(eventDetails, CreationFailed)
      }
    case DeployVersion(commandId, containerId, version) =>
      val eventKey = containerId.eventKey append version.number
      eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandId)), version, DeploymentInProgress)
      val jsch = new JSch()
      val session = jsch.getSession(configuration.login, configuration.host, configuration.port)
      session.setPassword(configuration.password)
      session.setConfig("StrictHostKeyChecking", "no")
      session.connect()
      val channel = session.openChannel("sftp").asInstanceOf[ChannelSftp]
      channel.connect()
      channel.cd(versionsDir(containerId))
      val output = channel.put(version.number + ".tar.bz2")
      output.write("Hello world!".getBytes)
      output.close()
      channel.disconnect()
      session.disconnect()
      eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandId)), version, DeploymentCompleted)
  }

  private def versionsDir(containerId: ContainerId): String = {
    s"mkdir -p /opt/easyrider/containers/${containerId.containerName}/versions"
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
    shell.connect()
    val output = IOUtils.toString(in)
    val exitStatus = shell.getExitStatus
    shell.disconnect()
    session.disconnect()
    (exitStatus, output)
  }
}

object SshNodeAgent {
  def apply(eventBus: ActorRef)() = Props(classOf[SshNodeAgent], eventBus)
}