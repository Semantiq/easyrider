package easyrider.infrastructure.ssh

import akka.actor.{ActorRef, Actor, Props}
import akka.event.LoggingReceive
import com.jcraft.jsch.{ChannelExec, UserInfo, JSch}
import easyrider.{EventKey, EventId, EventDetails}
import easyrider.Infrastructure._
import easyrider.infrastructure.ssh.SshInfrastructure.NodeConfiguration
import org.apache.commons.io.IOUtils

class SshNodeAgent(eventBus: ActorRef, nodeConfiguration: NodeConfiguration) extends Actor {
  override def preStart() {
    eventBus ! NodeUpdatedEvent(EventDetails(EventId.generate(), EventKey(nodeConfiguration.id.id), Seq()))
  }

  override def receive = LoggingReceive {
    case CreateContainer(commandId, _, containerId) =>
      def eventDetails = EventDetails(EventId.generate(), containerId.eventKey, Seq(commandId))
      val jsch = new JSch()
      val session = jsch.getSession(nodeConfiguration.login, nodeConfiguration.host, nodeConfiguration.port)
      session.setPassword(nodeConfiguration.password)
      session.connect()
      val shell = session.openChannel("exec").asInstanceOf[ChannelExec]
      shell.setCommand(s"mkdir -p /opt/easyrider/containers/${containerId.containerName}")
      val in = shell.getInputStream
      shell.connect()
      IOUtils.copy(in, System.out)
      shell.getExitStatus match {
        case 0 =>
          eventBus ! ContainerStateChangedEvent(eventDetails, Created)
        case other =>
          eventBus ! ContainerStateChangedEvent(eventDetails, CreationFailed)
      }
      shell.disconnect()
      session.disconnect()
  }
}

object SshNodeAgent {
  def apply(eventBus: ActorRef)(nodeConfiguration: NodeConfiguration) = Props(classOf[SshNodeAgent], eventBus, nodeConfiguration)
}
