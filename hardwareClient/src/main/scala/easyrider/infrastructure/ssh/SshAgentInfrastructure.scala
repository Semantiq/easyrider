package easyrider.infrastructure.ssh

import akka.actor.{Actor, Props}
import akka.event.LoggingReceive
import easyrider.ComponentId

class SshAgentInfrastructure extends Actor {
  import easyrider.Infrastructure._
  var nodes = Set[NodeId]()

  override def receive = LoggingReceive {
    case FindNodes(queryId) =>
      sender ! FindNodesResult(ComponentId("SshInfrastructure"), queryId, nodes.toSeq)
    case CreateContainer(commandId) => println("not yet")
  }
}

object SshAgentInfrastructure {
  def apply() = Props(classOf[SshAgentInfrastructure])
}
