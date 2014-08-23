package easyrider.business.http

import akka.actor._
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

  def businessLogic: Receive = {
    val api = context.actorOf(apiFactory(self));
    implicit val formats = Serialization.formats(FullTypeHints(List(classOf[AnyRef])))

    {
      case x: AnyRef if sender() == api =>
        send(TextFrame(write[AnyRef](x)))
      case TextFrame(byteString) =>
        val json = byteString.utf8String
        api ! read[AnyRef](json)
      case x: FrameCommandFailed =>
        // TODO: Maybe it should stop this actor?
        log.error("frame command failed", x)
      case x: HttpRequest => // Possibly nothing to do
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
}