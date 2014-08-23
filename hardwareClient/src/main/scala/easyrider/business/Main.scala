package easyrider.business

import akka.actor.ActorSystem
import easyrider.business.core.CoreModule
import easyrider.business.http.HttpModule

object Main extends App {
  val actorSystem = ActorSystem("EasyRider")
  val core = new CoreModule(actorSystem)
  val http = new HttpModule(actorSystem, core.apiFactory)
}
