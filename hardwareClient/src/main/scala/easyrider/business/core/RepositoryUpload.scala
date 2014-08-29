package easyrider.business.core

import java.io.{FileOutputStream, File}

import akka.actor.{Props, ActorRef, Actor}
import akka.event.LoggingReceive
import akka.util.ByteString
import easyrider.{EventId, EventDetails, CommandId}
import easyrider.Repository.{Version, VersionAvailableEvent, UploadChunk, UploadCompleted}
import org.apache.commons.io.IOUtils

class RepositoryUpload(commandId: CommandId, version: Version, eventBus: ActorRef, repositoryDir: File) extends Actor {
  val applicationDir = new File(repositoryDir, version.applicationId.id)
  applicationDir.mkdirs()
  val versionFile = new FileOutputStream(new File(applicationDir, version.number + ".tar.bz2"))

  override def receive = LoggingReceive {
    case UploadChunk(data: ByteString) =>
      // TODO: use Akka IO
      IOUtils.copy(data.iterator.asInputStream, versionFile)
    case UploadCompleted() =>
      versionFile.close()
      // TODO: validate archive, repackage to extract static web files & metadata
      eventBus ! VersionAvailableEvent(EventDetails(EventId.generate(), version.eventKey, Seq(commandId)), version)
  }
}

object RepositoryUpload {
  def apply(eventBus: ActorRef)(commandId: CommandId, version: Version, repositoryDir: File) = Props(classOf[RepositoryUpload], commandId,
    version, eventBus, repositoryDir)
}