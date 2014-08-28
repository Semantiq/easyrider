package easyrider.business.http

import akka.actor._
import akka.io.IO
import spray.can.Http
import spray.can.server.UHttp

class WebServer(port: Int, workerFactory: ActorRef => Props) extends Actor {
  override def preStart() {
    implicit val system = context.system
    IO(UHttp) ! Http.Bind(self, "localhost", port)
  }
  def receive = {
    case Http.Connected(remoteAddress, localAddress) =>
      val serverConnection = sender()
      val conn = context.actorOf(workerFactory(serverConnection))
      serverConnection ! Http.Register(conn)
  }
}

object WebServer {
  def apply(port: Int, workerFactory: ActorRef => Props) = Props(classOf[WebServer], port, workerFactory)
}
