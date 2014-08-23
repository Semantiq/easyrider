package easyrider.business

import akka.actor.ActorSystem
import easyrider.business.core.CoreModule

object Main extends App {
  val actorSystem = ActorSystem("EasyRider")
  val core = new CoreModule(actorSystem)
}
