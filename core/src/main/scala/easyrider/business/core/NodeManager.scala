package easyrider.business.core

import akka.actor.{Actor, ActorRef, Props}
import easyrider.Infrastructure.{NodeId, NodeUpdatedEvent}
import easyrider.Plugins.NotifyNodeStatus
import easyrider.RemoteAccess.RemoteAccessCommand
import easyrider.{EventDetails, EventId, EventKey}

class NodeManager(eventBus: ActorRef) extends Actor {
  var nodes = Map[NodeId, ActorRef]()

  override def receive: Receive = {
    case NotifyNodeStatus(commandDetails, nodeId, nodeStatus) =>
      nodes += nodeId -> sender()
      eventBus ! NodeUpdatedEvent(EventDetails(EventId.generate(), EventKey(nodeId.id), Seq()), nodeId, nodeStatus)
    case command: RemoteAccessCommand =>
      nodes.get(command.nodeId) match {
        case Some(node) => node.forward(command)
        case None => command.failure("Unknown node: " + command.nodeId)
      }
  }
}

object NodeManager {
  def apply(eventBus: ActorRef) = Props(classOf[NodeManager], eventBus)
}