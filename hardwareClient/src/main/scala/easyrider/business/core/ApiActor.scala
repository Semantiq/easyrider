package easyrider.business.core

import akka.actor._
import easyrider.Applications.ApplicationCommand
import easyrider.Components.ComponentCommand
import easyrider.Events.EventBusCommand
import easyrider.Repository.StartUpload
import easyrider.SshInfrastructure.SshInfrastructureCommand
import easyrider._

class ApiActor(bus: ActorRef, applicationManager: ActorRef, componentManager: ActorRef, sshInfrastructure: ActorRef,
               repositoryStorage: ActorRef, client: ActorRef) extends Actor {
  import easyrider.Api._

  def receive = {
    case AuthenticateUser() =>
      val auth = Authentication()
      context.become(authenticated(auth))
      client ! auth
    case AuthenticateComponent(componentId) =>
      val auth = Authentication()
      context.become(authenticated(auth))
      componentManager ! ComponentManager.Register(componentId)
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
    case f: Failure =>
      client ! f
  }

  val processCommand: Command => Unit = {
    case c: StartUpload =>
      repositoryStorage.forward(c)
    case c: ApplicationCommand =>
      applicationManager ! c
    case c: EventBusCommand =>
      bus ! c
    case c: ComponentCommand if sender() == client =>
      componentManager ! c
    case c: ComponentCommand =>
      client ! c
    case c: SshInfrastructureCommand =>
      sshInfrastructure ! c
  }
}

object ApiActor {
  def apply(bus: ActorRef, applicationManager: ActorRef, componentManager: ActorRef, sshInfrastructure: ActorRef,
            repositoryStorage: ActorRef)(client: ActorRef) = Props(classOf[ApiActor], bus, applicationManager,
            componentManager, sshInfrastructure, repositoryStorage, client)
}
