package easyrider.infrastructure.ssh

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.Api.{AuthenticateComponent, AuthenticateUser, Authentication}
import easyrider.Components.{ComponentCommand, ConsoleExtension, ConsoleExtensionAvailableEvent, ExtensionId}
import easyrider.{ComponentId, EventDetails, EventId}

class SshInfrastructurePlugin(apiFactory: ActorRef => Props) extends Actor {
  val componentId = ComponentId(classOf[SshInfrastructurePlugin].getName)
  val api = context.actorOf(apiFactory(self), "Api")
  api ! AuthenticateComponent(componentId)

  def authenticating = LoggingReceive {
    case Authentication() => becomeRunning()
  }

  def running = LoggingReceive {
    case ComponentCommand(commandId, _, payload) =>
  }

  def becomeRunning() {
    val extension = ConsoleExtension(componentId, new ExtensionId("hostsManager"))
    api ! ConsoleExtensionAvailableEvent(EventDetails(EventId.generate(), extension.eventKey, Seq()), extension)
    context.become(running)
  }

  override def receive = authenticating
}
