package easyrider.business.core.builtin

import java.io.{File, FileOutputStream}

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import akka.util.ByteString
import easyrider.Repository._
import easyrider._
import org.apache.commons.io.IOUtils

class RepositoryUpload(commandId: CommandId, version: Version, repository: ActorRef, repositoryDir: File) extends Actor {
  val applicationDir = RepositoryStorageLayout.applicationDir(repositoryDir, version)
  applicationDir.mkdirs()
  private val versionFileName = RepositoryStorageLayout.versionFileName(repositoryDir, version)
  val versionFile = new FileOutputStream(versionFileName)

  override def receive = LoggingReceive {
    case UploadChunk(data: ByteString) =>
      // TODO: use Akka IO
      IOUtils.copy(data.iterator.asInputStream, versionFile)
    case UploadCompleted() =>
      versionFile.close()
      // TODO: validate archive, repackage to extract static web files & metadata
      repository ! NotifyNewVersion(CommandDetails(), VersionMetadata(version, Seq(), RepositoryUpload.builtinPackageType))
      //VersionAvailableEvent(EventDetails(EventId.generate(), version.eventKey, Seq(commandId)), version)
  }
}

object RepositoryUpload {
  def apply(repository: ActorRef)(commandId: CommandId, version: Version, repositoryDir: File) = Props(classOf[RepositoryUpload], commandId,
    version, repository, repositoryDir)

  val builtinPackageType = PackageType("builtin")
}
