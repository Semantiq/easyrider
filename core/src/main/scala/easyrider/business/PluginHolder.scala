package easyrider.business

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.Event
import easyrider.Events.EventBusCommand
import easyrider.Infrastructure.ContainerCommand

class PluginHolder(eventBus: ActorRef, applicationManager: ActorRef, pluginProps: Props) extends Actor {
  val plugin = context.actorOf(pluginProps, "plugin")
  override def receive = LoggingReceive {
    case c: EventBusCommand => eventBus ! c
    case c: ContainerCommand => applicationManager ! c
    case e: Event => plugin ! e
  }
}

object PluginHolder {
  def apply(eventBus: ActorRef, applicationManager: ActorRef)(plugin: Props) = Props(classOf[PluginHolder], eventBus, applicationManager, plugin)
}
