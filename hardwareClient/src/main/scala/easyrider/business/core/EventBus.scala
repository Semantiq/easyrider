package easyrider.business.core

import akka.actor.{Actor, ActorRef, Props, Terminated}
import akka.event.LoggingReceive
import easyrider.{Event, EventKey, EventType}
import easyrider.Implicits._

class EventBus extends Actor {
  import easyrider.Events._
  private case class Subscription(eventType: EventType, eventKey: EventKey, receiver: ActorRef, subscriptionId: String)
  private var snapshots = Map[EventType, Map[EventKey, Event]]()
  private var subscriptions = Set[Subscription]()

  override def receive: Receive = LoggingReceive {
    case event: Event =>
      val current = snapshots.getOrElse(event.getClass, Map())
      val updated = if (event.eventDetails.removal) {
        current - event.eventDetails.eventKey
      } else {
        current.updated(event.eventDetails.eventKey, event)
      }
      snapshots += (class2eventType(event.getClass) -> updated)
      subscriptions
        .filter(s => s.eventType == class2eventType(event.getClass) && s.eventKey.contains(event.eventDetails.eventKey))
        .foreach(s => s.receiver ! event)
    case Terminated(subscriber) =>
      subscriptions = subscriptions.filter(s => s.receiver != subscriber)
    case command: EventBusCommand => command match {
      case Subscribe(commandId, subscriptionId, eventType, eventKey) =>
        val snapshot = snapshots.getOrElse(eventType, Map()).values
          .filter(event => eventKey.contains(event.eventDetails.eventKey))
          .toSeq
        sender() ! Subscribed(command.queryId, subscriptionId, eventType, snapshot)
        subscriptions += Subscription(eventType, eventKey, sender(), subscriptionId)
        context.watch(sender())
      case command @ UnSubscribe(commandId, subscriptionId) =>
        sender() ! UnSubscribed(command.queryId, subscriptionId)
        subscriptions = subscriptions.filter(s => s.subscriptionId != subscriptionId)
    }
  }
}

object EventBus {
  def apply() = Props(classOf[EventBus])
}
