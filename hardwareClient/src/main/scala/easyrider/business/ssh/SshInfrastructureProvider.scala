package easyrider.business.ssh

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import easyrider.Events.{GetSnapshot, GetSnapshotResponse}
import easyrider.Implicits._
import easyrider.Infrastructure._
import SshInfrastructure.{CreateNode, NodeConfigurationUpdatedEvent}
import easyrider.{CommandId, ComponentId, QueryId}

import scala.concurrent.duration._

class SshInfrastructureProvider(eventBus: ActorRef, sshNodeAgent: () => Props) extends Actor {
  var nodes = Map[NodeId, ActorRef]()

  implicit val timeout = Timeout(3 seconds)
  implicit val dispatcher = context.system.dispatcher
  eventBus ? GetSnapshot(QueryId.generate(), classOf[NodeConfigurationUpdatedEvent]) pipeTo self

  def initializing: Receive = {
    case GetSnapshotResponse(_, events: Seq[NodeConfigurationUpdatedEvent]) =>
      nodes = events.map { event =>
        val agent = context.actorOf(sshNodeAgent(), event.nodeConfiguration.id.id)
        agent ! CreateNode(CommandId.generate(), event.nodeConfiguration)
        event.nodeConfiguration.id -> agent
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
