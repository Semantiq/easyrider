package easyrider.business

import akka.actor.Actor
import akka.event.LoggingReceive
import easyrider.Event

class EventBus extends Actor {
  import easyrider.EventBus._
  override def receive: Receive = LoggingReceive {
    case event: Event => ???
    case command: EventBusCommand => command match {
      case Subscribe(subscriptionId, eventType, eventKey) => ???
      case UnSubscribe(subscriptionId) => ???
    }
  }
}
