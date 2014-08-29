package easyrider.business.core

import java.io.File

import akka.actor.{Actor, Props}
import akka.event.LoggingReceive
import easyrider.CommandId
import easyrider.Repository.{StartUpload, Upload, Version}

class RepositoryStorage(easyriderData: File, uploadFactory: (CommandId, Version, File) => Props) extends Actor {
  val repositoryDir = new File(easyriderData, "repository")
  repositoryDir.mkdirs()

  override def receive = LoggingReceive {
    case StartUpload(commandId, version) =>
      val upload = context.actorOf(uploadFactory(commandId, version, repositoryDir))
      sender() ! Upload(upload)
  }
}

object RepositoryStorage {
  def apply(easyriderData: File, uploadFactory: (CommandId, Version, File) => Props) = Props(classOf[RepositoryStorage],
    easyriderData: File, uploadFactory)
}
