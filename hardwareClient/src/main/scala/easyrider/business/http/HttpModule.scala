package easyrider.business.http

import akka.actor.{ActorRef, Props, ActorSystem}
import akka.io.IO
import spray.can.Http
import spray.can.server.UHttp

class HttpModule(system: ActorSystem, apiFactory: ActorRef => Props) {
  val port = 8080

  val server = system.actorOf(WebServer(port, apiFactory))
}