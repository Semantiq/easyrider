package easyrider.business.core

import akka.actor.{Props, ActorRef, Actor}
import akka.event.LoggingReceive
import akka.util.ByteString
import easyrider.{EventId, EventDetails, CommandId}
import easyrider.Repository.{Version, VersionAvailableEvent, UploadChunk, UploadCompleted}

class RepositoryUpload(commandId: CommandId, version: Version, eventBus: ActorRef) extends Actor {
  override def receive = LoggingReceive {
    case UploadChunk(data: ByteString) =>
    case UploadCompleted() =>
      eventBus ! VersionAvailableEvent(EventDetails(EventId.generate(), version.eventKey, Seq(commandId)), version)
  }
}

object RepositoryUpload {
  def apply(eventBus: ActorRef)(commandId: CommandId, version: Version) = Props(classOf[RepositoryUpload], commandId,
    version, eventBus)
}
