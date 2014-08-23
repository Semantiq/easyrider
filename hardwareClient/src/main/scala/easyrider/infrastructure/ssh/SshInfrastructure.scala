package easyrider.infrastructure.ssh

import akka.actor.{ActorRef, Actor, Props}
import akka.event.LoggingReceive
import easyrider.ComponentId
import easyrider.Infrastructure._
import easyrider.infrastructure.ssh.SshInfrastructure.AddNode

class SshInfrastructure extends Actor {
  var nodes = Map[NodeId, ActorRef]()

  override def receive = LoggingReceive {
    case FindNodes(queryId) =>
      sender ! FindNodesResult(ComponentId("SshInfrastructure"), queryId, nodes.keys.toSeq)
    case CreateContainer(commandId) => println("not yet")
  }
}

object SshInfrastructure {
  def apply() = Props(classOf[SshInfrastructure])

  case class AddNode(id: NodeId, host: String, port: Int, login: String, password: String)
}
