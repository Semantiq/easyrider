package eu.semantiq.easyrider

import akka.actor.{ActorLogging, ActorRef, Props, Actor}
import spray.can.server.websockets.Sockets
import spray.can.server.websockets.model.{OpCode, Frame}
import akka.util.ByteString
import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write
import eu.semantiq.easyrider.StatusMonitor.{GetStatus, Status}

class ApiController(statusMonitor: ActorRef) extends Actor with ActorLogging {
  implicit val formats = Serialization.formats(NoTypeHints)

  def initializing: Receive = {
    case Sockets.Upgraded =>
      context.become(running(sender))
      context.system.eventStream.subscribe(self, classOf[Status])
      statusMonitor ! GetStatus
  }

  def running(peer: ActorRef): Receive = {
    case event: Status => peer ! Frame(opcode = OpCode.Text, data = ByteString(write(event)))
  }

  def receive: Receive = initializing
}

object ApiController {
  def apply(statusMonitor: ActorRef) = Props(classOf[ApiController], statusMonitor)
}
