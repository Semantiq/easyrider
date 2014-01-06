package eu.semantiq.easyrider.console

import akka.actor.{Props, ActorRef, Actor}
import akka.io.{Tcp, IO}
import spray.can.server.websockets.Sockets
import spray.can.Http
import spray.can.Http.Register

class HttpDispatcher(statusMonitor: ActorRef) extends Actor {
  import HttpDispatcher._
  private implicit val system = context.system

  def initializing: Receive = {
    case NewConfiguration(port: Int) =>
      IO(Sockets) ! Http.Bind(self, "0.0.0.0", port)
      context.become(started)
  }

  def started: Receive = {
    case _: Tcp.Connected => sender ! Register(context.actorOf(StatusController(statusMonitor)))
  }


  def receive: Receive = initializing
}

object HttpDispatcher {
  def apply(statusMonitor: ActorRef) = Props(classOf[HttpDispatcher], statusMonitor)

  case class NewConfiguration(port: Int)
}