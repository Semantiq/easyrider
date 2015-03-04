package easyrider.business.http

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.util.Timeout
import easyrider.PageProvider

import scala.concurrent.duration._
import scala.language.postfixOps

class HttpModule(system: ActorSystem, apiFactory: ActorRef => Props, port: Int, pages: Seq[PageProvider]) {
  // TODO: use pages in template engine
  val timeout = Timeout(5 seconds)
  val workersRegistry = system.actorOf(HttpWorkersRegistry(), "HttpWorkersRegistry")
  val workerFactory = WebServerWorker(apiFactory, workersRegistry, timeout) _
  val server = system.actorOf(WebServer(port, workerFactory))
}
