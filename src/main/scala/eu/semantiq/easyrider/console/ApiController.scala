package eu.semantiq.easyrider.console

import akka.actor.{ActorLogging, ActorRef, Props, Actor}
import akka.util.ByteString
import spray.can.server.websockets.Sockets
import spray.can.server.websockets.model.{OpCode, Frame}
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write
import eu.semantiq.easyrider.StatusMonitor
import eu.semantiq.easyrider.supervisor.AppSupervisor
import eu.semantiq.easyrider.builder.AppBuilder

// TODO: separate from WebSocket handling code
class ApiController(statusMonitor: ActorRef) extends Actor with ActorLogging {
  implicit val formats = Serialization.formats(FullTypeHints(List(
    classOf[AppSupervisor.AppLifecycleCommand],
    classOf[AppSupervisor.AppLifecycleEvent],
    classOf[AppBuilder.BuildEvent])))

  def initializing: Receive = {
    case Sockets.Upgraded =>
      context.become(running(sender))
      context.system.eventStream.subscribe(self, classOf[StatusMonitor.Status])
      context.system.eventStream.subscribe(self, classOf[StatusMonitor.AuditEntry])
      statusMonitor ! StatusMonitor.GetStatus
  }

  def running(peer: ActorRef): Receive = {
    case event: StatusMonitor.Status => peer ! Frame(opcode = OpCode.Text, data = ByteString(write(event)))
    case event: StatusMonitor.AuditEntry => peer ! Frame(opcode = OpCode.Text, data = ByteString(write(event)))
    case Frame(_, _, OpCode.Text, _, data) =>
      val command = parse(data.utf8String).extract[AppSupervisor.AppLifecycleCommand]
      context.system.eventStream.publish(command)
  }

  def receive: Receive = initializing
}

object ApiController {
  def apply(statusMonitor: ActorRef) = Props(classOf[ApiController], statusMonitor)
}
