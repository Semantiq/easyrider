package easyrider.infrastructure.ssh

import akka.actor.{ActorRef, Props, Actor}
import akka.event.LoggingReceive
import easyrider.Api.Authentication
import easyrider.Components.ComponentCommand

class SshInfrastructurePlugin(apiFactory: ActorRef => Props) extends Actor {
  val api = context.actorOf(apiFactory(self), "Api")

  def authenticating = LoggingReceive {
    case Authentication() => becomeRunning()
  }

  def running = LoggingReceive {
    case ComponentCommand(commandId, _, payload) =>
  }

  def becomeRunning() {
    api !
    context.become(running)
  }

  override def receive = authenticating
}
