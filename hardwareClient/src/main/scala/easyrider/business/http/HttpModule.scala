package easyrider.business.http

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.util.Timeout

import scala.concurrent.duration._

class HttpModule(system: ActorSystem, apiFactory: ActorRef => Props, port: Int) {
  val timeout = Timeout(5 seconds)
  val workerFactory = WebServerWorker(apiFactory, timeout) _
  val server = system.actorOf(WebServer(port, workerFactory))
}
