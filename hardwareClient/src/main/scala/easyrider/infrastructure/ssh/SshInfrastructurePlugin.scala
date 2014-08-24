package easyrider.infrastructure.ssh

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.Api.{Authenticate, Authentication}
import easyrider.Components.{ComponentCommand, ConsoleExtension, ConsoleExtensionAvailableEvent, ExtensionId}
import easyrider.{ComponentId, EventDetails, EventId, EventKey}

class SshInfrastructurePlugin(apiFactory: ActorRef => Props) extends Actor {
  val componentId = ComponentId(classOf[SshInfrastructurePlugin].getName)
  val api = context.actorOf(apiFactory(self), "Api")
  api ! Authenticate()

  def authenticating = LoggingReceive {
    case Authentication() => becomeRunning()
  }

  def running = LoggingReceive {
    case ComponentCommand(commandId, _, payload) =>
  }

  def becomeRunning() {
    api ! ConsoleExtensionAvailableEvent(EventDetails(EventId.generate(), EventKey(), Seq()), ConsoleExtension(componentId, new ExtensionId("hostsManager")))
    context.become(running)
  }

  override def receive = authenticating
}
