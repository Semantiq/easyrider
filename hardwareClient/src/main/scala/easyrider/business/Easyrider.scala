package easyrider.business

import java.io.File

import akka.actor.ActorSystem
import easyrider.business.core.CoreModule
import easyrider.business.http.HttpModule

class Easyrider(port: Int, easyriderData: File) {
  val actorSystem = ActorSystem("EasyRider")
  val core = new CoreModule(easyriderData, actorSystem)
  val http = new HttpModule(actorSystem, core.apiFactory, port)
}
