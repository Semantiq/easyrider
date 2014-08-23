package easyrider.business.http

import akka.actor._
import akka.io.IO
import spray.can.Http
import spray.can.server.UHttp

class WebServer(port: Int, apiFactory: ActorRef => Props) extends Actor {
  override def preStart {
    implicit val system = context.system
    IO(UHttp) ! Http.Bind(self, "localhost", port)
  }
  def receive = {
    case Http.Connected(remoteAddress, localAddress) =>
      val serverConnection = sender()
      val conn = context.actorOf(WebServerWorker(serverConnection, apiFactory))
      serverConnection ! Http.Register(conn)
  }
}

object WebServer {
  def apply(port: Int, apiFactory: ActorRef => Props) = Props(classOf[WebServer], port, apiFactory)
}
