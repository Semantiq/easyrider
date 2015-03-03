package easyrider.business.core.builtin

import java.io.File

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.Applications.ApplicationId
import easyrider.Events.{SnapshotSubscriptionStarted, SnapshotUpdatedEvent, StartSnapshotSubscription}
import easyrider.Repository._
import easyrider._
import easyrider.business.core.builtin.RepositoryDirectoryLayout

class RepositoryStorage(val easyriderData: File,
                        uploadFactory: (CommandId, Version, File) => Props,
                        downloadFactory: (Version, ActorRef) => Props,
                        eventBus: ActorRef) extends Actor with RepositoryDirectoryLayout {
  repositoryDir.mkdirs()
  eventBus ! StartSnapshotSubscription(CommandDetails(), SnapshotEntryType(classOf[VersionMetadata]))

  override def receive = LoggingReceive {
    case StartUpload(commandDetails, version) =>
      val upload = context.actorOf(uploadFactory(commandDetails.commandId, version, repositoryDir))
      sender() ! Upload(upload)
    case StartDownload(version) =>
      context.actorOf(downloadFactory(version, sender()))
    case e: SnapshotSubscriptionStarted[_] =>
      // TODO
    case SnapshotUpdatedEvent(_, _, SnapshotUpdateDetails(_, eventKey, None)) =>
      val Seq(applicationId, number) = eventKey.key
      RepositoryStorageLayout.versionFileName(repositoryDir, Version(ApplicationId(applicationId), number)).delete()
  }
}

object RepositoryStorage {
  def apply(easyriderData: File,
            uploadFactory: (CommandId, Version, File) => Props,
            downloadFactory: (Version, ActorRef) => Props,
            eventBus: ActorRef) = Props(classOf[RepositoryStorage], easyriderData: File, uploadFactory, downloadFactory, eventBus)
}
