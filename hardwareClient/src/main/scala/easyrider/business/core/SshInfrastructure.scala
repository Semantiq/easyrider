package easyrider.business.core

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import akka.util.Timeout
import easyrider.{CommandId, QueryId, ComponentId}
import easyrider.Events.{GetSnapshotResponse, GetSnapshot}
import easyrider.Infrastructure._
import easyrider.SshInfrastructure.{NodeConfigurationUpdatedEvent, CreateNode}

import scala.concurrent.duration._
import akka.pattern.ask
import akka.pattern.pipe

import easyrider.Implicits._

class SshInfrastructure(eventBus: ActorRef, sshNodeAgent: () => Props) extends Actor {
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

object SshInfrastructure {
  def apply(eventBus: ActorRef, sshNodeAgent: () => Props) = Props(classOf[SshInfrastructure], eventBus, sshNodeAgent)
}
