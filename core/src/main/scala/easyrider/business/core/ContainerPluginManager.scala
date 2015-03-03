package easyrider.business.core

import akka.actor._
import akka.event.LoggingReceive
import easyrider.Infrastructure.{CreateContainer, AddressedContainerCommand}
import easyrider.Plugins.RegisterContainerPlugin

class ContainerPluginManager extends Actor with ActorLogging {
  private var containerPlugins = Map[String, ActorRef]()

  override def receive = LoggingReceive {
    case command @ RegisterContainerPlugin(commandDetails, name) =>
      containerPlugins += name -> sender()
      context.watch(sender())
    case Terminated(terminatedPlugin) =>
      containerPlugins.find(_._2 == terminatedPlugin) match {
        case Some((name, _)) =>
          log.info("Un-registering terminated container plugin: {}", name)
          containerPlugins -= name
        case None =>
          // received termination of an actor that's not a plugin - ignore
      }
    case AddressedContainerCommand(containerType, nodeId, containerCommand) =>
      containerPlugins.get(containerType) match {
        case Some(plugin) => plugin.forward(containerCommand)
        case None => sender() ! containerCommand.failure("Unknown container type: " + containerType)
      }
    case command: CreateContainer =>
      val containerType = "builtin"
      containerPlugins.get(containerType) match {
        case Some(plugin) => plugin.forward(command)
        case None => sender() ! command.failure(s"Cannot create container of type $containerType")
      }
  }
}

object ContainerPluginManager {
  def apply() = Props(classOf[ContainerPluginManager])
}
