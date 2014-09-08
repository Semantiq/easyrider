package easyrider.business.core

import java.io.File

import akka.actor.{ActorRef, Actor, Props}
import akka.event.LoggingReceive
import easyrider.CommandId
import easyrider.Repository.{StartDownload, StartUpload, Upload, Version}

class RepositoryStorage(val easyriderData: File,
                        uploadFactory: (CommandId, Version, File) => Props,
                        downloadFactory: (Version, ActorRef) => Props) extends Actor with RepositoryDirectoryLayout {
  repositoryDir.mkdirs()

  override def receive = LoggingReceive {
    case StartUpload(commandId, version) =>
      val upload = context.actorOf(uploadFactory(commandId, version, repositoryDir))
      sender() ! Upload(upload)
    case StartDownload(version) =>
      context.actorOf(downloadFactory(version, sender()))
  }
}

object RepositoryStorage {
  def apply(easyriderData: File,
            uploadFactory: (CommandId, Version, File) => Props,
            downloadFactory: (Version, ActorRef) => Props) = Props(classOf[RepositoryStorage], easyriderData: File, uploadFactory, downloadFactory)
}
