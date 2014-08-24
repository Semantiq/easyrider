package easyrider.business.http

import akka.actor._
import easyrider.Events.Subscribed
import spray.can.server.UHttp
import spray.can.{Http, websocket}
import spray.can.websocket.{FrameCommand, FrameCommandFailed}
import spray.can.websocket.frame.{Frame, TextFrame}
import spray.http.HttpRequest
import spray.routing.HttpServiceActor
import org.json4s.native.Serialization
import org.json4s._
import org.json4s.native.Serialization.{write, read}

/*class EventKeyMapSerializer extends CustomSerializer[Map[_, _]](format => (
  {
    ???
  }, {
  implicit val formats = Serialization.formats(FullTypeHints(List(classOf[AnyRef]))) + new EventKeyMapSerializer;

  {
    case x: Map[String, _] =>
      Extraction.decompose(x)
    case x: Map[_, _] =>
      JArray(x.toSeq.map { case (key, value) =>
        JObject(JField("key", Extraction.decompose(key)) :: JField("value", Extraction.decompose(value)) :: Nil)
      }.toList)
  }
}
))*/

object ToJsonFriendly {
  val convert: AnyRef => AnyRef = {
    /*case s: Subscribed =>
      s.*/
    case x => x
  }
}

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
        println(x)
        send(TextFrame(write[AnyRef](ToJsonFriendly.convert(x))))
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