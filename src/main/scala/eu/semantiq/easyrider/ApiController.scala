package eu.semantiq.easyrider

import akka.actor.{ActorLogging, ActorRef, Props, Actor}
import spray.can.server.websockets.Sockets
import spray.can.server.websockets.model.{OpCode, Frame}
import akka.util.ByteString
import eu.semantiq.easyrider.AppSupervisor.AppLifecycleEvent
import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write

class ApiController extends Actor with ActorLogging {
  implicit val formats = Serialization.formats(FullTypeHints(List(classOf[AppLifecycleEvent])))

  def initializing: Receive = {
    case Sockets.Upgraded =>
      context.become(running(sender))
      context.system.eventStream.subscribe(self, classOf[AppLifecycleEvent])
      sender ! Frame(opcode = OpCode.Text, data = ByteString("Hello!"))
  }

  def running(peer: ActorRef): Receive = {
    case event: AppLifecycleEvent => peer ! Frame(opcode = OpCode.Text, data = ByteString(write(event)))
  }

  def receive: Receive = initializing
}

object ApiController {
  def apply() = Props(classOf[ApiController])
}
