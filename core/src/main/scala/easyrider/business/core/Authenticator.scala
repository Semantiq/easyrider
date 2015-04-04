package easyrider.business.core

import akka.actor.{Props, Actor}
import easyrider.Api.{AuthenticationFailure, AuthenticateUser, Authentication}
import easyrider.{EventDetails, EventId}
import easyrider.business.Configuration
import org.apache.commons.codec.digest.DigestUtils

class Authenticator extends Actor {
  override def receive: Receive = {
    case AuthenticateUser(commandDetails, username, password) =>
      // TODO: move to builtin plugin and delegate
      if (username == "admin" && DigestUtils.sha512Hex(password) == Configuration.builtinPasswordHash) {
        sender ! Authentication(EventDetails(EventId.generate()), username, None, commandDetails.commandId)
      } else {
        sender ! AuthenticationFailure(EventDetails(EventId.generate()), commandDetails.commandId)
      }
  }
}

object Authenticator {
  def apply() = Props(classOf[Authenticator])
}
