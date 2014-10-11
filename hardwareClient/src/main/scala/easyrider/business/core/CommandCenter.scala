package easyrider.business.core

import akka.actor.{ActorLogging, ActorRef, Props, Actor}
import akka.event.LoggingReceive
import easyrider.Api.CommandSentEvent
import easyrider.{EventKey, EventId, EventDetails, Command}
import easyrider.Commands.{Failure, Success, RegisterProvider}

class CommandCenter(eventBus: ActorRef) extends Actor with ActorLogging {
  var providers = Map[Class[_ <: Command], ActorRef]()

  override def receive = LoggingReceive {
    case RegisterProvider(commandClass) =>
      providers += (commandClass -> sender)
    case command: Command =>
      providers.find(_._1.isAssignableFrom(command.getClass)) match {
        case Some((_, provider)) =>
          provider ! command
          eventBus ! CommandSentEvent(EventDetails(EventId.generate(), EventKey(), Seq(command.commandDetails.commandId)), command)
        case None => ???
      }
    case success: Success => eventBus ! success
    case failure: Failure => eventBus ! failure
  }
}

object CommandCenter {
  def apply(eventBus: ActorRef) = Props(classOf[CommandCenter], eventBus)
}
