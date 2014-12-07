package easyrider.business.ssh

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import easyrider.Events.{GetSnapshot, GetSnapshotResponse}
import easyrider.Implicits._
import easyrider.Infrastructure._
import easyrider._
import easyrider.business.ssh.SshInfrastructure.{CommandAndSubscribe, CreateNode, NodeConfigurationUpdatedEvent}
import org.reactivestreams.Subscriber

import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.util.Success

class SshInfrastructureProvider(eventBus: ActorRef, sshNodeAgent: () => Props) extends Actor {
  import easyrider.business.ssh.SshInfrastructureProvider._
  var nodes = Map[NodeId, ActorRef]()
  var subscribers = Map[SubscriptionId, Subscriber[NodeUpdatedEvent]]()

  implicit val timeout = Timeout(3 seconds)
  implicit val dispatcher = context.system.dispatcher
  eventBus ? GetSnapshot(QueryId.generate(), classOf[NodeConfigurationUpdatedEvent]) pipeTo self

  def initializing: Receive = {
    case GetSnapshotResponse(_, events: Seq[NodeConfigurationUpdatedEvent]) =>
      nodes = events.map { event =>
        val agent = context.actorOf(sshNodeAgent(), event.nodeConfiguration.id.id)
        agent ! CreateNode(CommandDetails(CommandId.generate(), TraceMode()), event.nodeConfiguration)
        event.nodeConfiguration.id -> agent
      }.toMap
      context.become(running)
  }

  def running = LoggingReceive {
    case SubscribeNodeUpdatedEvent(subscriber, promise) =>
      val subscription = NodeUpdatedSubscription(SubscriptionId.generate(), self)
      subscribers += subscription.id -> subscriber
      subscriber.onSubscribe(subscription)
      promise.complete(Success(Seq()))
    case event: NodeUpdatedEvent =>
      eventBus ! event
      subscribers.values.foreach(s => s.onNext(event))
    case Cancel(subscriptionId: SubscriptionId) =>
      subscribers -= subscriptionId
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
    case AddressedContainerCommand(nodeId, message, subscriber) =>
      (nodes.get(nodeId), subscriber) match {
        case (Some(node), Some(sub)) =>
          node.forward(CommandAndSubscribe(message, sub))
        case (Some(node), None) =>
          node.forward(message)
        case (None, _) =>
          sender ! message.failure(s"Node ${nodeId.id} does not exist")
      }
  }

  override def receive = initializing
}

object SshInfrastructureProvider {
  def apply(eventBus: ActorRef, sshNodeAgent: () => Props) = Props(classOf[SshInfrastructureProvider], eventBus, sshNodeAgent)

  case class SubscribeNodeUpdatedEvent(subscriber: Subscriber[NodeUpdatedEvent], promise: Promise[Seq[NodeUpdatedEvent]])
}
