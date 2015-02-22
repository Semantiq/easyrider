package easyrider.business.ssh

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.Events.{GetSnapshot, GetSnapshotResponse, Snapshot}
import easyrider.Infrastructure._
import easyrider._
import easyrider.business.ssh.SshInfrastructure.{CreateNode, NodeConfiguration}

class SshInfrastructureProvider(eventBus: ActorRef, sshNodeAgent: () => Props) extends Actor {
  var nodes = Map[NodeId, ActorRef]()

  eventBus ! GetSnapshot(QueryId.generate(), SnapshotEntryType(classOf[NodeConfiguration]))

  def initializing = LoggingReceive {
    case GetSnapshotResponse(_, Snapshot(_, entries)) =>
      nodes = entries.values.map(_.asInstanceOf[NodeConfiguration]).map { nodeConfiguration =>
        val agent = context.actorOf(sshNodeAgent(), nodeConfiguration.id.id)
        agent ! CreateNode(CommandDetails(), nodeConfiguration)
        nodeConfiguration.id -> agent
      }.toMap
      context.become(running)
  }

  def running = LoggingReceive {
    case addNode: CreateNode if !nodes.contains(addNode.nodeConfiguration.id) =>
      val node = context.actorOf(sshNodeAgent(), addNode.nodeConfiguration.id.id)
      nodes += (addNode.nodeConfiguration.id -> node)
      node ! addNode
    case FindNodes(queryId) =>
      sender ! FindNodesResult(ComponentId("SshInfrastructure"), queryId, nodes.keys.toSeq)
    case command @ CreateContainer(commandId, nodeId, _) => nodes.get(nodeId) match {
      case Some(nodeAgent) => nodeAgent ! command
      case None => command.failure(s"Unknown node ${nodeId.id}")
    }
    case AddressedContainerCommand(nodeId, message) =>
      nodes.get(nodeId) match {
        case Some(node) =>
          node.forward(message)
        case None =>
          sender ! message.failure(s"Node ${nodeId.id} does not exist")
      }
  }

  override def receive = initializing
}

object SshInfrastructureProvider {
  def apply(eventBus: ActorRef, sshNodeAgent: () => Props) = Props(classOf[SshInfrastructureProvider], eventBus, sshNodeAgent)
}
