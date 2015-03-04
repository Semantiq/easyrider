package easyrider.builtin

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.Applications.{ContainerConfiguration, ContainerId}
import easyrider.Events.{GetSnapshot, GetSnapshotResponse, Snapshot}
import easyrider.Infrastructure._
import easyrider._

class Containers(eventBus: ActorRef, containerAgent: ContainerConfiguration => Props) extends Actor {
  var containers = Map[ContainerId, ActorRef]()

  eventBus ! GetSnapshot(CommandDetails(), SnapshotEntryType(classOf[ContainerConfiguration]))

  def initializing = LoggingReceive {
    case GetSnapshotResponse(_, Snapshot(_, entries), _, _) =>
      containers = entries.values.map(_.asInstanceOf[ContainerConfiguration]).map { containerConfiguration =>
        val agent = context.actorOf(containerAgent(containerConfiguration), containerConfiguration.id.containerName)
        containerConfiguration.id -> agent
      }.toMap
      context.become(running)
  }

  def running = LoggingReceive {
    case command @ CreateContainer(commandId, nodeId, containerId) => containers.get(containerId) match {
      case Some(agent) => sender() ! command.failure(s"Container ${containerId.id} already exists")
      case None =>
        // TODO: use actual configuration, in case it's required by the agent
        val agent = context.actorOf(containerAgent(ContainerConfiguration(containerId, nodeId, Seq())), containerId.containerName)
        containers += containerId -> agent
        agent.forward(command)
    }
    case command: ContainerCommand =>
      containers.get(command.containerId) match {
        case Some(node) =>
          node.forward(command)
        case None =>
          sender ! command.failure(s"Container ${command.containerId.id} does not exist")
      }
  }

  override def receive = initializing
}

object Containers {
  def apply(eventBus: ActorRef, containerAgent: ContainerConfiguration => Props) = Props(classOf[Containers], eventBus, containerAgent)
}
