package easyrider.infrastructure.ssh

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.Api.{AuthenticateComponent, Authentication}
import easyrider.Components.{ComponentCommand, ConsoleExtension, ConsoleExtensionAvailableEvent, ExtensionId}
import easyrider.Infrastructure.NodeId
import easyrider.infrastructure.ssh.SshInfrastructure.NodeConfiguration
import easyrider.{ComponentId, EventDetails, EventId}

class SshInfrastructurePlugin(apiFactory: ActorRef => Props, infrastructure: ActorRef) extends Actor {
  import SshInfrastructurePlugin._

  val api = context.actorOf(apiFactory(self), "Api")
  api ! AuthenticateComponent(componentId)

  def authenticating = LoggingReceive {
    case Authentication() => becomeRunning()
  }

  def running = LoggingReceive {
    case ComponentCommand(commandId, _, payload) =>
      val message = payload("type") match {
        case "NodeConfiguration" => NodeConfiguration(NodeId(payload("id")), payload("host"), payload("port").toInt, payload("login"), payload("password"))
      }
      infrastructure ! message
  }

  def becomeRunning() {
    val extension = ConsoleExtension(componentId, new ExtensionId("hostsManager"))
    api ! ConsoleExtensionAvailableEvent(EventDetails(EventId.generate(), extension.eventKey, Seq()), extension)
    context.become(running)
  }

  override def receive = authenticating
}

object SshInfrastructurePlugin {
  val componentId = ComponentId(classOf[SshInfrastructurePlugin].getName)
  def apply(apiFactory: ActorRef => Props, infrastructure: ActorRef) = Props(classOf[SshInfrastructurePlugin], apiFactory, infrastructure)
}
