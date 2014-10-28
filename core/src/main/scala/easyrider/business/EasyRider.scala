package easyrider.business

import java.io.File
import java.net.URL

import akka.actor.ActorSystem
import easyrider.business.core.CoreModule
import easyrider.business.http.HttpModule

class EasyRider(port: Int, easyRiderData: File) {
  val easyRiderUrl = new URL(s"http://localhost:$port")
  val actorSystem = ActorSystem("EasyRider")
  val core = new CoreModule(easyRiderData, easyRiderUrl, actorSystem)
  val http = new HttpModule(actorSystem, core.apiFactory, port)
}
