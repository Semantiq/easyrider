package easyrider.business.ssh

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.Events.{Snapshot, GetSnapshot, GetSnapshotResponse}
import easyrider.Infrastructure.NodeId
import easyrider.RemoteAccess.{RemoteAccessCommand, RemoteAccessEvent}
import easyrider.business.ssh.SshInfrastructure._
import easyrider.{PluginFactory, QueryId, SnapshotEntryType}

class UnixServerInfrastructureFactory extends PluginFactory {
  private def sshSessionFactory(config: NodeConfiguration, parent: ActorRef) = SshSession(parent, parent)(config)
  def props = UnixServerInfrastructure(sshSessionFactory)
}

class UnixServerInfrastructure(sshSession: (NodeConfiguration, ActorRef) => Props) extends Actor with ActorLogging {
  var nodes = Map[NodeId, ActorRef]()

  context.parent ! GetSnapshot(QueryId.generate(), SnapshotEntryType(classOf[NodeConfiguration]))

  def initializing = LoggingReceive {
    case GetSnapshotResponse(_, Snapshot(_, entries)) =>
      nodes = entries.values.map(_.asInstanceOf[NodeConfiguration]).map { nodeConfiguration =>
        val session = context.actorOf(sshSession(nodeConfiguration, self), nodeConfiguration.id.id)
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
