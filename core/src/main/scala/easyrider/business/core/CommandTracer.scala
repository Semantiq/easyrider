package easyrider.business.core

import akka.actor.{ActorLogging, Actor, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.Commands.Success
import easyrider._
import easyrider.Events.{EventDeliveryComplete, EventDelivered, SubscribeToCommandTrail}

class CommandTracer(subscriber: ActorRef, command: SubscribeToCommandTrail) extends Actor with ActorLogging {
  var trackedCauses = Set[Cause](command.commandId)
  context.watch(subscriber)

  override def receive = LoggingReceive {
    case event: Event =>
      if (event.eventDetails.causedBy.exists(cause => trackedCauses contains cause)) {
        subscriber ! EventDelivered(EventDetails(EventId.generate(), EventKey(), Seq(command.commandDetails.commandId)), event)
        // TODO: optionally: trackedCauses += event.eventDetails.eventId
        if (event.isInstanceOf[Success]) {
          // TODO: allow tracing to continue until terminated by user
          subscriber ! EventDeliveryComplete(EventDetails(EventId.generate(), EventKey(), Seq(command.commandDetails.commandId)))
          context.stop(self)
        }
      }
  }
}

object CommandTracer {
  def apply(subscriber: ActorRef, command: SubscribeToCommandTrail) = Props(classOf[CommandTracer], subscriber, command)
}
