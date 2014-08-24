package easyrider.infrastructure.ssh

import akka.actor.{Props, ActorRef, ActorSystem}

class SshInfrastructureModule(system: ActorSystem, eventBus: ActorRef, apiFactory: ActorRef => Props) {
  val infrastructure = system.actorOf(SshInfrastructure(SshNodeAgent(eventBus)), "SshInfrastructure")
  val plugin = system.actorOf(SshInfrastructurePlugin(apiFactory, infrastructure))
}
