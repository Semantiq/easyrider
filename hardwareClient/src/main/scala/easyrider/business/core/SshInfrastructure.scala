package easyrider.business.core

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.ComponentId
import easyrider.Infrastructure._
import easyrider.SshInfrastructure.CreateNode

class SshInfrastructure(sshNodeAgent: () => Props) extends Actor {
  var nodes = Map[NodeId, ActorRef]()

  override def receive = LoggingReceive {
    case addNode: CreateNode if !nodes.contains(addNode.nodeConfiguration.id) =>
      val node = context.actorOf(sshNodeAgent())
      nodes += (addNode.nodeConfiguration.id -> node)
      node ! addNode
    case FindNodes(queryId) =>
      sender ! FindNodesResult(ComponentId("SshInfrastructure"), queryId, nodes.keys.toSeq)
    case command @ CreateContainer(commandId, nodeId, _) => nodes.get(nodeId) match {
      case Some(nodeAgent) => nodeAgent ! command
      case None => command.failure(s"Unknown node ${nodeId.id}")
    }
  }
}

object SshInfrastructure {
  def apply(sshNodeAgent: () => Props) = Props(classOf[SshInfrastructure], sshNodeAgent)
}
