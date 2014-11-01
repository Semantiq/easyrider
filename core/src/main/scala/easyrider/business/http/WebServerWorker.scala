package easyrider.business.http

import java.lang.reflect.InvocationTargetException

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import easyrider.Api.AuthenticateUser
import easyrider.Applications.ApplicationId
import easyrider.{TraceMode, CommandDetails, CommandId}
import easyrider.Repository._
import easyrider.business.http.WebServerWorker.MessageFormatError
import org.json4s._
import org.json4s.ext.JodaTimeSerializers
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}
import spray.can.websocket
import spray.can.websocket.FrameCommandFailed
import spray.can.websocket.frame.TextFrame
import spray.http.{HttpRequest, MultipartFormData, StatusCodes}
import spray.routing.HttpServiceActor

class WebServerWorker(connection: ActorRef, apiFactory: ActorRef => Props, implicit val timeout: Timeout) extends HttpServiceActor
  with websocket.WebSocketServerWorker {
  implicit val dispatcher = context.system.dispatcher

  def serverConnection = connection
  override def receive = handshaking orElse businessLogicNoUpgrade orElse closeLogic

  val api = context.actorOf(apiFactory(self))
  context.watch(api)

  def businessLogic: Receive = {
    implicit val formats = Serialization.formats(FullTypeHints(List(classOf[AnyRef]))) ++ JodaTimeSerializers.all

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

  def businessLogicNoUpgrade(): Receive = {
    implicit val refFactory: ActorRefFactory = context
    runRoute {
      pathPrefix("api") {
         pathPrefix("repository") {
           path("upload") {
             post {
               entity(as[MultipartFormData]) { formData =>
                 (formData.get("content"), formData.get("application"), formData.get("version")) match {
                   case (Some(content), Some(name), Some(version)) =>
                     val api = context.actorOf(apiFactory(self))
                     api ! AuthenticateUser()
                     val commandId = CommandId.generate()
                     val upload = api ? StartUpload(CommandDetails(commandId, TraceMode()), Version(ApplicationId(name.entity.asString), version.entity.asString))
                     upload.onSuccess {
                       case Upload(target) =>
                         context.stop(api)
                         content.entity.data.toChunkStream(2048).foreach(e => target ! UploadChunk(e.toByteString))
                         target ! UploadCompleted()
                     }
                     complete(commandId.id)
                   case x =>
                     respondWithStatus(StatusCodes.BadRequest) {
                       complete("Error: " + x)
                     }
                 }
               }
             }
           }
         }
      } ~
      getFromResourceDirectory("static") ~ getFromResource("static/index.html") // TODO: run index through template engine
    }
  }
}

object WebServerWorker {
  def apply(apiFactory: ActorRef => Props, timeout: Timeout)(connection: ActorRef) = Props(classOf[WebServerWorker], connection, apiFactory, timeout)

  case class MessageFormatError(message: String)
}