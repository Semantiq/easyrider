package easyrider.business.http

import java.lang.reflect.InvocationTargetException

import akka.actor._
import akka.util.Timeout
import easyrider.business.core.EventBusSerializers
import easyrider.business.http.WebServerWorker.MessageFormatError
import org.json4s._
import org.json4s.ext.JodaTimeSerializers
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}
import spray.can.websocket
import spray.can.websocket.FrameCommandFailed
import spray.can.websocket.frame.TextFrame
import spray.http._
import spray.routing.HttpServiceActor

class WebServerWorker(connection: ActorRef, apiFactory: ActorRef => Props,
                      workersRegistry: ActorRef, implicit val timeout: Timeout) extends HttpServiceActor
  with websocket.WebSocketServerWorker {
  implicit val dispatcher = context.system.dispatcher

  def serverConnection = connection
  override def receive = handshaking orElse upload orElse businessLogicNoUpgrade orElse closeLogic

  val api = context.actorOf(apiFactory(self))
  context.watch(api)

  def businessLogic: Receive = {
    implicit val formats = Serialization.formats(FullTypeHints(List(classOf[AnyRef]))) ++ JodaTimeSerializers.all ++ EventBusSerializers.serializers

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

  def upload(): Receive = {
    // TODO: introduce a URL pattern for all plugins
    case r @ HttpRequest(_, uri, _, _, _) if uri.path.startsWith(Uri.Path("/api/repository/")) =>
      // TODO: use actual plugin name from URL pattern
      workersRegistry.forward(HttpWorkersRegistry.Handle("BuiltinPluginFactory", r))
  }

  def businessLogicNoUpgrade(): Receive = {
    implicit val refFactory: ActorRefFactory = context
    runRoute {
      path("api" / "logOut") {
        deleteCookie("authObject", path = "/") {
          redirect(Uri("/"), StatusCodes.TemporaryRedirect)
        }
      } ~
      getFromResourceDirectory("static") ~ getFromResource("static/index.html") // TODO: run index through template engine
    }
  }
}

object WebServerWorker {
  def apply(apiFactory: ActorRef => Props, workersRegistry: ActorRef, timeout: Timeout)(connection: ActorRef) = Props(classOf[WebServerWorker],
    connection, apiFactory, workersRegistry, timeout)

  case class MessageFormatError(message: String)
}
