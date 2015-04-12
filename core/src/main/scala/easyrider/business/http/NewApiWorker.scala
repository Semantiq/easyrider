package easyrider.business.http

import java.lang.reflect.InvocationTargetException

import akka.actor.{ActorRef, Props, Terminated}
import akka.util.Timeout
import easyrider.Event
import easyrider.business.http.WebServerWorker.MessageFormatError
import easyrider.json.JsonSerializer
import org.json4s.MappingException
import org.json4s.native.Serialization._
import spray.can.websocket
import spray.can.websocket.FrameCommandFailed
import spray.can.websocket.frame.TextFrame
import spray.routing.HttpServiceActor

class NewApiWorker(connection: ActorRef, apiFactory: ActorRef => Props,
                   implicit val timeout: Timeout) extends HttpServiceActor with websocket.WebSocketServerWorker {
  val api = context.actorOf(apiFactory(self), "api")
  val serializer = new JsonSerializer()
  def serverConnection = connection

  def businessLogic: Receive = {
    case Terminated(actor) if actor == api =>
      context.stop(self)
    case x: Event if sender() == api =>
      val message = serializer.write(x)
      log.info("client << {}", message)
      send(TextFrame(message))
    case TextFrame(byteString) =>
      try {
        val message = byteString.utf8String
        log.info("client >> {}", message)
        val apiMessage = serializer.readCommand(message)
        api ! apiMessage
      } catch {
        case e: MappingException =>
          val message = e.getCause match {
            case ite: InvocationTargetException => ite.getCause.getMessage
            case _ => e.getMessage
          }
          log.error(e, message)
          //send(TextFrame(write[AnyRef](MessageFormatError(message))))
      }
    case x: FrameCommandFailed =>
      // TODO: Maybe it should stop this actor?
      log.error("frame command failed", x)
  }
}

object NewApiWorker {
  def apply(apiFactory: ActorRef => Props, timeout: Timeout)(connection: ActorRef) =
    Props(new NewApiWorker(connection, apiFactory, timeout))
}
