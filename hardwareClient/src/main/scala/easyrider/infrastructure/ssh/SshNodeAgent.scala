package easyrider.infrastructure.ssh

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import com.jcraft.jsch.{ChannelExec, JSch}
import easyrider.Infrastructure._
import easyrider.infrastructure.ssh.SshInfrastructure.{CreateNode, NodeConfiguration}
import easyrider.{EventDetails, EventId, EventKey}
import org.apache.commons.io.IOUtils

class SshNodeAgent(eventBus: ActorRef, nodeConfiguration: NodeConfiguration) extends Actor {
  override def receive = LoggingReceive {
    case CreateNode =>
      runSshCommand("mkdir -p easyrider")
      eventBus ! NodeUpdatedEvent(EventDetails(EventId.generate(), EventKey(nodeConfiguration.id.id), Seq()))
    case CreateContainer(commandId, _, containerId) =>
      def eventDetails = EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId))
      val (exitStatus, output) = runSshCommand(s"mkdir -p /opt/easyrider/containers/${containerId.containerName}")
      println(output)
      exitStatus match {
        case 0 =>
          eventBus ! ContainerStateChangedEvent(eventDetails, Created)
        case other =>
          eventBus ! ContainerStateChangedEvent(eventDetails, CreationFailed)
      }
  }

  private def runSshCommand(command: String) = {
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
  def apply(eventBus: ActorRef)(nodeConfiguration: NodeConfiguration) = Props(classOf[SshNodeAgent], eventBus, nodeConfiguration)
}
