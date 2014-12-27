package easyrider.business.core

import java.util.concurrent.TimeUnit

import akka.actor._
import easyrider.Applications.ApplicationCommand
import easyrider.Commands.Failure
import easyrider.Components.ComponentCommand
import easyrider.Events.{EventBusCommand, GetReplay}
import easyrider.Infrastructure.ContainerCommand
import easyrider.Orchestrator.OrchestrationCommand
import easyrider.Repository.StartUpload
import easyrider._
import easyrider.business.ssh.SshInfrastructure.SshInfrastructureCommand

import scala.concurrent.duration.Duration

class ApiActor(bus: ActorRef, applicationManager: ActorRef, componentManager: ActorRef, sshInfrastructure: ActorRef,
               repositoryStorage: ActorRef, client: ActorRef, orchestrator: ActorRef,
               authenticator: ActorRef) extends Actor with Stash with ActorLogging {
  import easyrider.Api._

  def receive = awaitingCredentials()

  def awaitingCredentials(): Receive = {
    case authenticationRequest: AuthenticateUser =>
      authenticator ! authenticationRequest
      context.become(authenticating())
      context.setReceiveTimeout(Duration(1, TimeUnit.SECONDS))
    case AuthenticateComponent(componentId) =>
      val auth = Authentication()
      context.become(authenticated(auth))
      componentManager ! ComponentManager.Register(componentId)
      client ! auth
    case message =>
      log.warning("Received message before authentication, disconnecting: {}", message)
      context.stop(self)
  }

  def authenticating(): Receive = {
    case auth @ Authentication() =>
      context.become(authenticated(auth))
      unstashAll()
      client ! auth
      context.setReceiveTimeout(Duration.Inf)
    case ReceiveTimeout => context.stop(self)
    case failure @ AuthenticationFailure() =>
      log.warning(s"Authentication failure for $client: $failure")
      client ! failure
      context.become(awaitingCredentials())
      context.setReceiveTimeout(Duration.Inf)
    case _ => stash()
  }

  def authenticated(authenticated: Authentication): Receive = {
    case e: Event =>
      if(sender() == bus)
        client ! e
      else
        bus ! e
    case c: Command =>
      bus ! CommandSentEvent(EventDetails(EventId.generate(), EventKey(), Seq(c.commandDetails.commandId)), c, Some(authenticated))
      processCommand(c)
    case r: GetReplay =>
      bus ! r
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
    case c: ContainerCommand =>
      applicationManager ! c
    case c: OrchestrationCommand =>
      orchestrator ! c
    case c: SshInfrastructureCommand =>
      sshInfrastructure ! c
  }
}

object ApiActor {
  def apply(bus: ActorRef, applicationManager: ActorRef, componentManager: ActorRef, sshInfrastructure: ActorRef,
            repositoryStorage: ActorRef, orchestrator: ActorRef, authenticator: ActorRef)(client: ActorRef) = Props(classOf[ApiActor],
            bus, applicationManager, componentManager, sshInfrastructure, repositoryStorage, client, orchestrator, authenticator)
}
