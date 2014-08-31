package easyrider.business.core

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import com.jcraft.jsch.{ChannelSftp, ChannelExec, JSch}
import easyrider.Applications.ContainerId
import easyrider.Infrastructure._
import easyrider.Repository.Version
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
      val (exitStatus, output) = runSshCommand(configuration, "mkdir -p " + versionsDir(containerId))
      println(output)
      exitStatus match {
        case 0 =>
          eventBus ! ContainerStateChangedEvent(eventDetails, ContainerCreated)
        case other =>
          eventBus ! ContainerStateChangedEvent(eventDetails, ContainerCreationFailed)
      }
    case DeployVersion(commandId, containerId, version) =>
      val eventKey = containerId.eventKey append version.number
      eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandId)), version, DeploymentInProgress)
      try {
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
      } catch {
        case ex: Exception =>
          // TODO: check what operation failed and make the message out of that
          // TODO: log the exception
          VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(commandId)), version, DeploymentFailed(ex.getMessage))
      }
    case StartContainer(commandId, containerId, version) =>
      // TODO: implement
      runSshCommand(configuration, "echo '" + version.number + "' > " + containerDir(containerId) + "/running.version")
      eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId)), ContainerRunning(version))
    case StopContainer(commandId, containerId, immediate) =>
      // TODO: implement
      val (0, runningVersionNumber) = runSshCommand(configuration, "cat " + containerDir(containerId) + "/running.version")
      eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId)), ContainerStopping(Version(containerId.stageId.applicationId, runningVersionNumber.trim)))
      Thread.sleep(1000)
      eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId)), ContainerCreated)
  }

  private def versionsDir(containerId: ContainerId) = {
    containerDir(containerId) + "/versions"
  }

  def containerDir(containerId: ContainerId) = {
    s"easyrider/containers/${containerId.containerName}"
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
    val output = "out='" + IOUtils.toString(in) + "' err='" + IOUtils.toString(err) + "'"
    val exitStatus = shell.getExitStatus
    shell.disconnect()
    session.disconnect()
    (exitStatus, output)
  }
}

object SshNodeAgent {
  def apply(eventBus: ActorRef)() = Props(classOf[SshNodeAgent], eventBus)
}
