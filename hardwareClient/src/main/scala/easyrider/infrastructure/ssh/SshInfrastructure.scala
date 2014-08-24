package easyrider.infrastructure.ssh

import akka.actor.{ActorRef, Actor, Props}
import akka.event.LoggingReceive
import easyrider.ComponentId
import easyrider.Infrastructure._
import easyrider.infrastructure.ssh.SshInfrastructure.NodeConfiguration

class SshInfrastructure(sshNodeAgent: NodeConfiguration => Props) extends Actor {
  var nodes = Map[NodeId, ActorRef]()

  override def receive = LoggingReceive {
    case addNode: NodeConfiguration if !nodes.contains(addNode.id) =>
      nodes += (addNode.id -> context.actorOf(sshNodeAgent(addNode)))
    case FindNodes(queryId) =>
      sender ! FindNodesResult(ComponentId("SshInfrastructure"), queryId, nodes.keys.toSeq)
    case command @ CreateContainer(commandId, nodeId, _) => nodes.get(nodeId) match {
      case Some(nodeAgent) => nodeAgent ! command
      case None => command.failure(s"Unknown node ${nodeId.id}")
    }
  }
}

object SshInfrastructure {
  def apply(sshNodeAgent: NodeConfiguration => Props) = Props(classOf[SshInfrastructure], sshNodeAgent)

  case class NodeConfiguration(id: NodeId, host: String, port: Int, login: String, password: String)
}
