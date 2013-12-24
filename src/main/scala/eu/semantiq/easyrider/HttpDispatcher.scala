package eu.semantiq.easyrider

import akka.actor.Actor
import akka.io.{Tcp, IO}
import spray.can.server.websockets.Sockets
import spray.can.Http
import spray.can.Http.Register
import eu.semantiq.easyrider.HttpDispatcher.NewConfiguration

class HttpDispatcher extends Actor {
  private implicit val system = context.system

  def initializing: Receive = {
    case NewConfiguration(port: Int) =>
      IO(Sockets) ! Http.Bind(self, "0.0.0.0", port)
      context.become(started)
  }

  def started: Receive = {
    case _: Tcp.Connected => sender ! Register(context.actorOf(StatusController()))
  }


  def receive: Receive = initializing
}

object HttpDispatcher {
  case class NewConfiguration(port: Int)
}
