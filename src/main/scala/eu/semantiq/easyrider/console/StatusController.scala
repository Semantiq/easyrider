package eu.semantiq.easyrider.console

import akka.actor._
import spray.can.server.websockets.Sockets
import spray.http._
import spray.http.StatusCode._
import spray.http.HttpMethods._
import org.apache.commons.io.IOUtils
import spray.http.HttpRequest
import spray.http.HttpResponse
import scala.Some
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy.Resume

class StatusController(statusMonitor: ActorRef) extends Actor with ActorLogging {
  private val htmlContentType = ContentType(MediaType.custom("text/html"), HttpCharset.custom("UTF-8"))
  private val cssContentType = ContentType(MediaType.custom("text/css"), HttpCharset.custom("UTF-8"))
  private val jsContentType = ContentType(MediaType.custom("application/javascript"), HttpCharset.custom("UTF-8"))
  private val woffContentType = ContentType(MediaType.custom("application/x-font-woff"), HttpCharset.custom("UTF-8"))
  private val contentTypeForExtension = Map("html" -> htmlContentType, "css" -> cssContentType, "js" -> jsContentType,
    "woff" -> woffContentType)

  override def supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1.minute) {
    case _: Exception => Resume
  }

  def receive = {
    case HttpRequest(GET, Uri.Path("/"), _, _, _) =>
      sender ! responseFromResource("/static/index.html")
    case req @ HttpRequest(GET, Uri.Path("/api"), _, _, _) =>
      sender ! Sockets.UpgradeServer(Sockets.acceptAllFunction(req), context.actorOf(ApiController(statusMonitor)))
    case HttpRequest(GET, Uri.Path(path), _, _, _) =>
      sender ! responseFromResource(s"/static$path")
  }

  private def responseFromResource(resourceName: String) = {
    val extension = resourceName.split("\\.").last
    Option(getClass.getResourceAsStream(resourceName)) match {
      case Some(input) => HttpResponse(200, HttpEntity(contentTypeForExtension(extension), IOUtils.toByteArray(input)))
      case None => HttpResponse(404, "Not found")
    }
  }
}

object StatusController {
  def apply(statusMonitor: ActorRef) = Props(classOf[StatusController], statusMonitor)
}
