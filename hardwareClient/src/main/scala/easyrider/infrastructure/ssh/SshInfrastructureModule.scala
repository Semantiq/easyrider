package easyrider.infrastructure.ssh

import akka.actor.{ActorRef, ActorSystem}

class SshInfrastructureModule(system: ActorSystem, eventBus: ActorRef) {
  val infrastructure = system.actorOf(SshInfrastructure(SshNodeAgent(eventBus)), "SshInfrastructure")
}
