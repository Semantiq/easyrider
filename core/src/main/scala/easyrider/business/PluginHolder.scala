package easyrider.business

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.Commands.{CommandExecution, Failure, Success}
import easyrider.{CommandId, Command, Event}
import easyrider.Events.{GetSnapshotResponse, GetSnapshot, EventBusCommand}
import easyrider.Infrastructure.ContainerCommand

class PluginHolder(eventBus: ActorRef, applicationManager: ActorRef, pluginProps: Props) extends Actor {
  val plugin = context.actorOf(pluginProps, "plugin")
  var commands = Map[CommandId, ActorRef]()

  override def receive = LoggingReceive {
    case c: EventBusCommand if sender == plugin => eventBus ! c
    case c: ContainerCommand if sender == plugin => applicationManager ! c
    case e: Event if sender != plugin => plugin ! e
    case c: GetSnapshot => eventBus ! c
    case r: GetSnapshotResponse[_] => plugin ! r
    case c: Command if sender != plugin =>
      plugin ! c
      commands += c.commandDetails.commandId -> sender
    case e: Event if sender == plugin =>
      eventBus ! e
      if (e.isInstanceOf[CommandExecution]) {
        commands.foreach {
          case (commandId, receiver) if e.eventDetails.causedBy contains commandId =>
            receiver ! e
            e match {
              case e: Success => commands -= commandId
              case e: Failure => commands -= commandId
              case _ =>
            }
          case _ => // ignore
        }
      }
  }
}

object PluginHolder {
  def apply(eventBus: ActorRef, applicationManager: ActorRef)(plugin: Props) = Props(classOf[PluginHolder], eventBus, applicationManager, plugin)
}
