package easyrider.business.core

import akka.actor.ActorRef
import easyrider.Commands.CommandExecution
import easyrider.Event

trait EventPublisher {
  def eventBus: ActorRef
  def sender(): ActorRef

  def publishEvent(event: Event): Unit = {
    eventBus ! event
    if (event.isInstanceOf[CommandExecution]) {
      sender() ! event
    }
  }
}
