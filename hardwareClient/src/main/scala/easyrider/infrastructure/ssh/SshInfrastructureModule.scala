package easyrider.infrastructure.ssh

import akka.actor.ActorSystem

class SshInfrastructureModule(system: ActorSystem) {
  val infrastructure = system.actorOf(SshInfrastructure(), "SshInfrastructure")
}
