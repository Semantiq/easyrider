package easyrider.business.core

import java.util.concurrent.TimeUnit

import akka.actor._
import akka.event.LoggingReceive
import easyrider.Applications.ApplicationCommand
import easyrider.Commands.Failure
import easyrider.Events.EventBusCommand
import easyrider.Infrastructure.ContainerCommand
import easyrider.Nodes.NodeManagementCommand
import easyrider.Orchestrator.OrchestrationCommand
import easyrider.Repository.RepositoryCommand
import easyrider._
import easyrider.business.Configuration
import org.apache.commons.codec.digest.DigestUtils

import scala.concurrent.duration.Duration

class ApiActor(bus: ActorRef, applicationManager: ActorRef, sshInfrastructure: ActorRef,
               client: ActorRef, orchestrator: ActorRef,
               authenticator: ActorRef, repository: ActorRef, nodeManager: ActorRef) extends Actor with Stash with ActorLogging {
  import easyrider.Api._

  def receive = awaitingCredentials()

  def awaitingCredentials(): Receive = {
    case authenticationRequest: AuthenticateUser =>
      authenticator ! authenticationRequest
      context.become(authenticating())
      context.setReceiveTimeout(Duration(1, TimeUnit.SECONDS))
    case request @ ReAuthenticateUser(user, signature) =>
      if (signature == saltedHash(user)) {
        val authentication = Authentication(user, Some(request))
        client ! authentication
        context.become(authenticated(authentication))
      } else {
        client ! AuthenticationFailure()
      }
    case message =>
      log.warning("Received message before authentication, disconnecting: {}", message)
      context.stop(self)
  }

  def authenticating(): Receive = {
    case auth @ Authentication(username, _) =>
      context.become(authenticated(auth))
      unstashAll()
      client ! auth.copy(authenticate = Some(ReAuthenticateUser(username, saltedHash(username))))
      context.setReceiveTimeout(Duration.Inf)
    case ReceiveTimeout => context.stop(self)
    case failure @ AuthenticationFailure() =>
      log.warning(s"Authentication failure for $client: $failure")
      client ! failure
      context.become(awaitingCredentials())
      context.setReceiveTimeout(Duration.Inf)
    case _ => stash()
  }

  def authenticated(authenticated: Authentication) = LoggingReceive {
    case e: Event =>
      if(sender() != client)
        client ! e
      else
        bus ! e
    case c: Command =>
      bus ! CommandSentEvent(EventDetails(EventId.generate()), c, Some(authenticated), c.commandDetails.commandId)
      processCommand(c)
    case r: Result =>
      client ! r
    case f: Failure =>
      client ! f
    case KeepAlive() =>
      // accept but do nothing
  }

  val processCommand: Command => Unit = {
    case c: RepositoryCommand =>
      repository ! c
    case c: ApplicationCommand =>
      applicationManager ! c
    case c: EventBusCommand =>
      bus ! c
    case c: ContainerCommand =>
      applicationManager ! c
    case c: OrchestrationCommand =>
      orchestrator ! c
    case c: NodeManagementCommand =>
      nodeManager ! c
  }

  private def saltedHash(string: String) = DigestUtils.sha512Hex(string + ":" + Configuration.builtinPasswordHash)
}

object ApiActor {
  def apply(bus: ActorRef, applicationManager: ActorRef, sshInfrastructure: ActorRef,
            orchestrator: ActorRef, authenticator: ActorRef, repository: ActorRef, nodeManager: ActorRef)(client: ActorRef) = Props(classOf[ApiActor],
            bus, applicationManager, sshInfrastructure, client, orchestrator, authenticator, repository, nodeManager)
}
