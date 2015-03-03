package easyrider.business.core.builtin

import akka.actor.{Actor, Props}
import spray.http.{HttpRequest, Uri}

class HttpWorker(uploadHandlerFactory: () => Props) extends Actor {
  override def receive: Receive = {
    case r@HttpRequest(_, Uri.Path("/api/repository/upload"), _, _, _) =>
      val handler = context.actorOf(uploadHandlerFactory())
      // TODO: this provides no back-pressure, will overflow with slow backend
      r.asPartStream(1024 * 10).foreach(handler.tell(_, sender()))
  }
}

object HttpWorker {
  def apply(uploadHandlerFactory: () => Props) = Props(classOf[HttpWorker], uploadHandlerFactory)
}
