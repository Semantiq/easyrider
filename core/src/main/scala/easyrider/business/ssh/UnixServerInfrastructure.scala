package easyrider.business.ssh

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.Events.{GetSnapshot, GetSnapshotResponse}
import easyrider.Implicits._
import easyrider.Infrastructure.NodeId
import easyrider.business.ssh.SshInfrastructure._
import easyrider.{PluginFactory, QueryId}

class UnixServerInfrastructureFactory extends PluginFactory {
  private def sshSessionFactory(config: NodeConfiguration, parent: ActorRef) = SshSession(parent, parent)(config)
  def props = UnixServerInfrastructure(sshSessionFactory)
}

class UnixServerInfrastructure(sshSession: (NodeConfiguration, ActorRef) => Props) extends Actor with ActorLogging {
  var nodes = Map[NodeId, ActorRef]()

  context.parent ! GetSnapshot(QueryId.generate(), classOf[NodeConfigurationUpdatedEvent])

  def initializing = LoggingReceive {
    case GetSnapshotResponse(_, events: Seq[NodeConfigurationUpdatedEvent]) =>
      nodes = events.map { event =>
        val session = context.actorOf(sshSession(event.nodeConfiguration, self), event.nodeConfiguration.id.id)
        //session ! CreateNode(CommandDetails(), event.nodeConfiguration)
        event.nodeConfiguration.id -> session
      }.toMap
      context.become(running)
  }

  def running = LoggingReceive {
    case addNode: CreateNode if !nodes.contains(addNode.nodeConfiguration.id) =>
      val node = context.actorOf(sshSession(addNode.nodeConfiguration, self), addNode.nodeConfiguration.id.id)
      nodes += (addNode.nodeConfiguration.id -> node)
      //node ! addNode
    case command @ RunSshCommand(_, nodeId, _) =>
      nodes(nodeId) ! command
    //case e: NodeConfigurationUpdatedEvent => context.parent ! e
    case e: SshEvent => context.parent ! e
  }

  override def receive = initializing
}

object UnixServerInfrastructure {
  def apply(sshSession: (NodeConfiguration, ActorRef) => Props) = Props(classOf[UnixServerInfrastructure], sshSession)
}