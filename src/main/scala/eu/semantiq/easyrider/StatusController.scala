package eu.semantiq.easyrider

import akka.actor.Actor
import eu.semantiq.easyrider.StatusController.NewConfiguration
import spray.can.server.websockets.Sockets
import akka.io.{Tcp, IO}
import spray.can.Http
import spray.can.Http.Register
import spray.http.HttpRequest
import spray.can.server.websockets.model.{OpCode, Frame}
import akka.util.ByteString
import scala.concurrent.duration._

class StatusController extends Actor {

  implicit val system = context.system

  def initializing: Receive = {
    case NewConfiguration(port: Int) =>
      IO(Sockets) ! Http.Bind(self, "0.0.0.0", port)
      context.become(started)
  }

  def started: Receive = {
    case x: Tcp.Connected => sender ! Register(self)
    case req: HttpRequest => sender ! Sockets.UpgradeServer(Sockets.acceptAllFunction(req), self)
    case Sockets.Upgraded => println("Upgrade successful")
    case f: Frame => sender ! Frame(opcode = OpCode.Text, data = ByteString("hello " + f.stringData))
  }

  def receive = initializing

}

object StatusController {
  case class NewConfiguration(port: Int)
}