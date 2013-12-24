package eu.semantiq.easyrider

import akka.actor.{Props, ActorLogging, Actor}
import spray.can.server.websockets.Sockets
import spray.http._
import spray.http.StatusCode._
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.HttpMethods._
import org.apache.commons.io.IOUtils

class StatusController extends Actor with ActorLogging {

  private val htmlContentType = ContentType(MediaType.custom("text/html"), HttpCharset.custom("UTF-8"))

  def receive = {
    case HttpRequest(GET, Uri.Path("/"), _, _, _) =>
      sender ! responseFromResource("/static/index.html")
    case req @ HttpRequest(GET, Uri.Path("/api"), _, _, _) =>
      sender ! Sockets.UpgradeServer(Sockets.acceptAllFunction(req), context.actorOf(ApiController()))
    case HttpRequest(GET, Uri.Path(path), _, _, _) =>
      sender ! responseFromResource(s"/static$path")
  }

  private def responseFromResource(resourceName: String) = {
    Option(getClass.getResourceAsStream(resourceName)) match {
      case Some(input) => HttpResponse(200, HttpEntity(htmlContentType, IOUtils.toByteArray(input)))
      case None => HttpResponse(404, "Not found")
    }
  }
}

object StatusController {
  def apply() = Props[StatusController]
}
