package easyrider.business.core

import akka.actor.{Actor, ActorRef, Props}
import easyrider.Infrastructure.NodeUpdatedEvent
import easyrider.Nodes.CreateNode
import easyrider.Plugins.{RegisterNodeManagementPlugin, NotifyNodeStatus}
import easyrider.RemoteAccess.RemoteAccessCommand
import easyrider.{NodeId, EventDetails, EventId, EventKey}

class NodeManager(eventBus: ActorRef) extends Actor {
  var nodes = Map[NodeId, ActorRef]()
  var nodeTypes = Map[String, ActorRef]()

  override def receive: Receive = {
    case NotifyNodeStatus(commandDetails, nodeId, nodeStatus) =>
      nodes += nodeId -> sender()
      eventBus ! NodeUpdatedEvent(EventDetails(EventId.generate(), EventKey(nodeId.id), Seq()), nodeId, nodeStatus)
    case RegisterNodeManagementPlugin(commandDetails, nodeType) =>
      nodeTypes += nodeType -> sender()
    case command: RemoteAccessCommand =>
      nodes.get(command.nodeId) match {
        case Some(node) => node.forward(command)
        case None => command.failure("Unknown node: " + command.nodeId)
      }
    case command @ CreateNode(commandDetails, nodeConfiguration) =>
      nodeTypes.get(nodeConfiguration.nodeType) match {
        case Some(handler) => handler.forward(command)
        case None => command.failure(s"Don't know how to create a node of type ${nodeConfiguration.nodeType}")
      }
  }
}

object NodeManager {
  def apply(eventBus: ActorRef) = Props(classOf[NodeManager], eventBus)
}