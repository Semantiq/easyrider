package easyrider.business

import akka.actor.ActorSystem
import easyrider.business.core.CoreModule
import easyrider.business.http.HttpModule
import easyrider.infrastructure.ssh.SshInfrastructureModule

class Easyrider(port: Int) {
  val actorSystem = ActorSystem("EasyRider")
  val core = new CoreModule(actorSystem)
  val sshInfrastructure = new SshInfrastructureModule(actorSystem, core.eventBus, core.apiFactory)
  val http = new HttpModule(actorSystem, core.apiFactory, port)
}
