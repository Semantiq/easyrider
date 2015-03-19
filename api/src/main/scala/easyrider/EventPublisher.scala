package easyrider

import akka.actor.ActorRef
import easyrider.Commands.CommandExecution

trait EventPublisher {
  def eventBus: ActorRef
  def sender(): ActorRef

  def publishEvent(event: Event, respondTo: ActorRef = sender()): Unit = {
    eventBus ! event
    if (event.isInstanceOf[CommandExecution]) {
      respondTo ! event
    }
  }
}
