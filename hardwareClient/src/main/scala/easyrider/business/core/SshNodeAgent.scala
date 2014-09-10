package easyrider.business.core

import akka.actor.{ActorLogging, Actor, ActorRef, Props}
import akka.event.LoggingReceive
import com.jcraft.jsch.{ChannelExec, JSch}
import easyrider.Applications.ContainerId
import easyrider.Infrastructure._
import easyrider.Repository.Version
import easyrider.SshInfrastructure.{CreateNode, NodeConfiguration, NodeConfigurationUpdatedEvent}
import easyrider.{EventDetails, EventId, EventKey}
import org.apache.commons.io.IOUtils
import org.slf4j.LoggerFactory

class SshNodeAgent(eventBus: ActorRef, deployerFactory: (NodeConfiguration, DeployVersion) => Props) extends Actor with SshNodeDirectoryLayout with ActorLogging {
  val containers = Set[ContainerId]()

  def unConfigured = LoggingReceive {
    case CreateNode(commandId, configuration) =>
      eventBus ! NodeUpdatedEvent(EventDetails(EventId.generate(), EventKey(configuration.id.id), Seq()), configuration.id, CreatingNode)
      eventBus ! NodeConfigurationUpdatedEvent(EventDetails(EventId.generate(), EventKey(configuration.id.id), Seq(commandId)), configuration)
      SshNodeAgent.runSshCommand(configuration, "mkdir -p easyrider")
      eventBus ! NodeUpdatedEvent(EventDetails(EventId.generate(), EventKey(configuration.id.id), Seq()), configuration.id, NodeCreated)
      context.become(configured(configuration))
  }

  def configured(configuration: NodeConfiguration) = LoggingReceive {
    case CreateContainer(commandId, _, containerId) =>
      def eventDetails = EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId))
      val (exitStatus, output, outputErr) = SshNodeAgent.runSshCommand(configuration, "mkdir -p " + versionsDir(containerId))
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
      // TODO: just a test
      val output = SshNodeAgent.runSshCommand(configuration, "sh " + versionsDir(containerId) + "/" + version.number + "/test_task.sh")
      log.info("Task execution result: {}", output)
      // TODO: implement
      SshNodeAgent.runSshCommand(configuration, "echo '" + version.number + "' > " + containerDir(containerId) + "/running.version")
      eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId)), ContainerRunning(version))
    case StopContainer(commandId, containerId, immediate) =>
      // TODO: implement
      val (0, runningVersionNumber, _) = SshNodeAgent.runSshCommand(configuration, "cat " + containerDir(containerId) + "/running.version")
      eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId)), ContainerStopping(Version(containerId.stageId.applicationId, runningVersionNumber.trim)))
      Thread.sleep(1000)
      eventBus ! ContainerStateChangedEvent(EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId)), ContainerCreated)
  }

  override def receive = unConfigured

}

object SshNodeAgent {
  def apply(eventBus: ActorRef, deployerFactory: (NodeConfiguration, DeployVersion) => Props)() = Props(classOf[SshNodeAgent], eventBus, deployerFactory)

  val log = LoggerFactory.getLogger(getClass)
  def runSshCommand(nodeConfiguration: NodeConfiguration, command: String) = {
    log.debug("Sending command to {}: {}", nodeConfiguration.id.id.asInstanceOf[Any], command)
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
    log.debug("Command output: {}", output)
    log.debug("Command error: {}", outputErr)
    log.debug("Command exit code: {}", exitStatus)
    (exitStatus, output, outputErr)
  }
}
