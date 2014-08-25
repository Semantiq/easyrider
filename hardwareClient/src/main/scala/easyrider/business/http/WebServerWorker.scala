package easyrider.business.http

import java.lang.reflect.InvocationTargetException

import akka.actor._
import easyrider.Events.Subscribed
import easyrider.business.http.WebServerWorker.MessageFormatError
import spray.can.server.UHttp
import spray.can.{Http, websocket}
import spray.can.websocket.{FrameCommand, FrameCommandFailed}
import spray.can.websocket.frame.{Frame, TextFrame}
import spray.http.HttpRequest
import spray.routing.HttpServiceActor
import org.json4s.native.Serialization
import org.json4s._
import org.json4s.native.Serialization.{write, read}

class WebServerWorker(connection: ActorRef, apiFactory: ActorRef => Props) extends HttpServiceActor
  with websocket.WebSocketServerWorker {

  def serverConnection() = connection
  override def receive = handshaking orElse businessLogicNoUpgrade orElse closeLogic

  val api = context.actorOf(apiFactory(self))
  context.watch(api)

  def businessLogic: Receive = {
    implicit val formats = Serialization.formats(FullTypeHints(List(classOf[AnyRef])))

    {
      case Terminated(actor) if actor == api =>
        context.stop(self)
      case x: AnyRef if sender() == api =>
        send(TextFrame(write[AnyRef](x)))
      case TextFrame(byteString) =>
        try {
          val apiMessage = read[AnyRef](byteString.utf8String)
          api ! apiMessage
        } catch {
          case e: MappingException =>
            val message = e.getCause match {
              case ite: InvocationTargetException => ite.getCause.getMessage
              case _ => e.getMessage
            }
            send(TextFrame(write[AnyRef](MessageFormatError(message))))
        }
      case x: FrameCommandFailed =>
        // TODO: Maybe it should stop this actor?
        log.error("frame command failed", x)
      case x: HttpRequest => // Possibly nothing to do
        println("got http request")
    }
  }

  def businessLogicNoUpgrade: Receive = {
    implicit val refFactory: ActorRefFactory = context
    runRoute {
      getFromResourceDirectory("static") ~ getFromResource("static/index.html")
    }
  }
}

object WebServerWorker {
  def apply(connection: ActorRef, apiFactory: ActorRef => Props) = Props(classOf[WebServerWorker], connection, apiFactory)

  case class MessageFormatError(message: String)
}