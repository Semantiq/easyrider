package easyrider.business.core

import java.io.File

import akka.actor.{ActorRef, Actor, Props}
import akka.event.LoggingReceive
import easyrider.{EventKey, EventId, EventDetails, CommandId}
import easyrider.Repository._

class RepositoryStorage(val easyriderData: File,
                        uploadFactory: (CommandId, Version, File) => Props,
                        downloadFactory: (Version, ActorRef) => Props,
                        eventBus: ActorRef) extends Actor with RepositoryDirectoryLayout {
  repositoryDir.mkdirs()

  override def receive = LoggingReceive {
    case StartUpload(commandDetails, version) =>
      val upload = context.actorOf(uploadFactory(commandDetails.commandId, version, repositoryDir))
      sender() ! Upload(upload)
    case StartDownload(version) =>
      context.actorOf(downloadFactory(version, sender()))
    case AddLabel(commandDetails, version, name) =>
      val label = Label(name, commandDetails.commandId)
      // TODO: implement
    case DeleteVersion(commandDetails, version) =>
      RepositoryStorageLayout.versionFileName(repositoryDir, version).delete()
      eventBus ! VersionAvailableEvent(EventDetails(EventId.generate(), EventKey(), Seq(commandDetails.commandId), removal = true), version)
  }
}

object RepositoryStorage {
  def apply(easyriderData: File,
            uploadFactory: (CommandId, Version, File) => Props,
            downloadFactory: (Version, ActorRef) => Props,
            eventBus: ActorRef) = Props(classOf[RepositoryStorage], easyriderData: File, uploadFactory, downloadFactory, eventBus)
}
