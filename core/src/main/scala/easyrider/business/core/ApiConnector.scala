package easyrider.business.core

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{Stash, ReceiveTimeout, Props, ActorRef}
import akka.event.LoggingReceive
import easyrider.Api.{AuthenticationFailure, Authentication, AuthenticateUser}
import easyrider.business.{Configuration, Connector}
import org.apache.commons.codec.digest.DigestUtils

import scala.concurrent.duration.Duration

class ApiConnector(eventBus: ActorRef, applicationManager: ActorRef,
                   containerPluginManager: ActorRef, nodeManager: ActorRef,
                   repository: ActorRef,
                   clientName: String, val client: ActorRef,
                   authenticator: ActorRef)
  extends Connector(eventBus, applicationManager, containerPluginManager, nodeManager, repository, clientName)
  with Stash {

  override def receive = notAuthenticated

  def notAuthenticated = LoggingReceive {
    case auth: AuthenticateUser =>
      authenticator ! auth
      context.become(authenticating)
      context.setReceiveTimeout(Duration(1, TimeUnit.SECONDS))
  }

  def authenticating = LoggingReceive {
    case auth: Authentication =>
      context.become(authenticated(auth))
      unstashAll()
      client ! auth.copy(signature = Some(saltedHash(auth.username)))
      context.setReceiveTimeout(Duration.Inf)
    case ReceiveTimeout => context.stop(self)
    case failure: AuthenticationFailure =>
      log.warning(s"Authentication failure for $client: $failure")
      client ! failure
      context.become(notAuthenticated)
      context.setReceiveTimeout(Duration.Inf)
    case _ => stash()
  }

  def authenticated(auth: Authentication) = super.receive

  // TODO: do this in authenticator
  private def saltedHash(string: String) = DigestUtils.sha512Hex(string + ":" + Configuration.builtinPasswordHash)
}

object ApiConnector {
  private val counter = new AtomicInteger(0)

  private def clientName = "client" + counter.getAndIncrement

  def apply(eventBus: ActorRef, applicationManager: ActorRef,
            containerPluginManager: ActorRef, nodeManager: ActorRef,
            repository: ActorRef, authenticator: ActorRef)(client: ActorRef) = Props(new ApiConnector(eventBus, applicationManager, containerPluginManager,
    nodeManager, repository, clientName, client, authenticator))
}
