package easyrider.business.core

import akka.actor._
import easyrider.Applications.ApplicationCommand
import easyrider.Events.EventBusCommand
import easyrider._

class ApiActor(bus: ActorRef, applicationManager: ActorRef, client: ActorRef) extends Actor {
  import easyrider.Api._

  def receive = {
    case Authenticate() =>
      val auth = Authentication()
      context.become(authenticated(auth))
      client ! auth
    case _ =>
      context.stop(self)
  }

  def authenticated(authenticated: Authentication): Receive = {
    case e: Event =>
      if(sender() == bus)
        client ! e
      else
        bus ! e
    case c: Command =>
      bus ! CommandSentEvent(EventDetails(EventId.generate(), EventKey(), Seq(c.commandId)), c, authenticated)
      processCommand(c)
    case q: Query =>
      ???
    case r: Result =>
      client ! r
  }

  val processCommand: Command => Unit = {
    case c: ApplicationCommand =>
      applicationManager ! c
    case c: EventBusCommand =>
      bus ! c
  }
}

object ApiActor {
  def apply(bus: ActorRef, applicationManager: ActorRef)(client: ActorRef) = Props(classOf[ApiActor], bus, applicationManager, client)
}