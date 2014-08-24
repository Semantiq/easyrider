package easyrider.business.core

import akka.actor.{Props, Terminated, Actor, ActorRef}
import easyrider.ComponentId
import easyrider.Components.ComponentCommand
import easyrider.business.core.ComponentManager.Register

class ComponentManager extends Actor {
  var components = Map[ComponentId, ActorRef]()

  override def receive: Receive = {
    case command : ComponentCommand =>
      components.get(command.componentId) match {
        case Some(component) => component.forward(command)
        case None => sender ! command.failure(s"${command.componentId}: extension unavailable")
      }
    case Register(componentId) =>
      components += (componentId -> sender)
      context.watch(sender())
    case Terminated(sender) =>
      components = components.filter(_._2 != sender)
  }
}

object ComponentManager {
  def apply() = Props(classOf[ComponentManager])

  case class Register(componentId: ComponentId)
}
