package easyrider.business.ssh

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.Events.{Snapshot, GetSnapshot, GetSnapshotResponse}
import easyrider.Infrastructure.{NodeCreated, NodeId}
import easyrider.Plugins.NotifyNodeStatus
import easyrider.RemoteAccess.{RemoteAccessCommand, RemoteAccessEvent}
import easyrider.business.ssh.SshInfrastructure._
import easyrider.{CommandDetails, PluginFactory, QueryId, SnapshotEntryType}

class UnixServerInfrastructureFactory extends PluginFactory {
  private def sshSessionFactory(config: NodeConfiguration, parent: ActorRef) = SshSession(parent)(config)
  def props = UnixServerInfrastructure(sshSessionFactory)
}

class UnixServerInfrastructure(sshSession: (NodeConfiguration, ActorRef) => Props) extends Actor with ActorLogging {
  var nodes = Map[NodeId, ActorRef]()

  context.parent ! GetSnapshot(CommandDetails(), SnapshotEntryType(classOf[NodeConfiguration]))

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
    case addNode: CreateNode if !nodes.contains(addNode.nodeConfiguration.id) =>
      val node = context.actorOf(sshSession(addNode.nodeConfiguration, self), addNode.nodeConfiguration.id.id)
      nodes += (addNode.nodeConfiguration.id -> node)
      //node ! addNode
    case command: RemoteAccessCommand =>
      // TODO: handle incorrect nodeId
      nodes(command.nodeId) ! command
    case e: RemoteAccessEvent => context.parent ! e
  }

  override def receive = initializing
}

object UnixServerInfrastructure {
  def apply(sshSession: (NodeConfiguration, ActorRef) => Props) = Props(classOf[UnixServerInfrastructure], sshSession)
}
