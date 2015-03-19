package easyrider.ssh

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.Events.{GetSnapshot, GetSnapshotResponse, Snapshot}
import easyrider.Infrastructure.NodeCreated
import easyrider.Nodes._
import easyrider.Plugins.{NotifyNodeStatus, RegisterNodeManagementPlugin}
import easyrider.RemoteAccess.{RemoteAccessCommand, RemoteAccessEvent}
import easyrider.{CommandDetails, PluginFactory, SnapshotEntryType, _}

class UnixServerInfrastructureFactory extends PluginFactory {
  private def sshSessionFactory(config: NodeConfiguration, parent: ActorRef) = SshSession(parent)(config)
  def props = UnixServerInfrastructure(sshSessionFactory)
}

class UnixServerInfrastructure(sshSession: (NodeConfiguration, ActorRef) => Props) extends Actor with ActorLogging {
  var nodes = Map[NodeId, ActorRef]()

  context.parent ! GetSnapshot(CommandDetails(), SnapshotEntryType(classOf[NodeConfiguration]))
  context.parent ! RegisterNodeManagementPlugin(CommandDetails(), "ssh")

  def initializing = LoggingReceive {
    case GetSnapshotResponse(_, Snapshot(_, entries), _, _) =>
      nodes = entries.values.map(_.asInstanceOf[NodeConfiguration]).map { nodeConfiguration =>
        val session = context.actorOf(sshSession(nodeConfiguration, self), nodeConfiguration.id.id)
        context.parent ! NotifyNodeStatus(CommandDetails(), nodeConfiguration.id, NodeCreated)
        nodeConfiguration.id -> session
      }.toMap
      context.become(running)
  }

  def running = LoggingReceive {
    case message if sender() == context.parent => message match {
      case addNode: CreateNode if !nodes.contains(addNode.nodeConfiguration.id) =>
        val node = context.actorOf(sshSession(addNode.nodeConfiguration, self), addNode.nodeConfiguration.id.id)
        nodes += (addNode.nodeConfiguration.id -> node)
        context.parent ! NodeConfigurationUpdatedEvent(EventDetails(EventId.generate(), EventKey(), Seq(addNode.commandDetails.commandId)), addNode.nodeConfiguration)
        context.parent ! NotifyNodeStatus(CommandDetails(), addNode.nodeConfiguration.id, NodeCreated)
      case command: RemoteAccessCommand =>
        // TODO: handle incorrect nodeId
        nodes(command.nodeId) ! command
    }
    case message if sender() != context.parent => message match {
      case e: RemoteAccessEvent => context.parent ! e
      case any => context.parent ! any
    }
  }

  override def receive = initializing
}

object UnixServerInfrastructure {
  def apply(sshSession: (NodeConfiguration, ActorRef) => Props) = Props(classOf[UnixServerInfrastructure], sshSession)
}
