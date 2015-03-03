package easyrider.business.http

import akka.actor._
import akka.event.LoggingReceive
import spray.http.{HttpEntity, StatusCodes, HttpResponse, HttpRequest}

class HttpWorkersRegistry extends Actor with ActorLogging {
  import easyrider.business.http.HttpWorkersRegistry._
  var handlers = Map[String, ActorRef]()

  override def receive = LoggingReceive {
    case Register(name, handler) =>
      handlers += name -> handler
      context.watch(handler)
    case Terminated(handler) =>
      handlers.find(_._2 == handler).map(_._1) foreach { name =>
        log.info("Http handler for {} terminated: {}", name, handler)
        handlers -= name
      }
    case Handle(name, request) =>
      handlers.get(name) match {
        case Some(handler) =>
          println("forwarding to " + handler)
          handler.forward(request)
        case None => HttpResponse(StatusCodes.NotFound, HttpEntity(s"Plugin $name not found or doesn't have http extension"))
      }
  }
}

object HttpWorkersRegistry {
  def apply() = Props(classOf[HttpWorkersRegistry])

  case class Register(name: String, handler: ActorRef)
  case class Handle(name: String, request: HttpRequest)
}
