package easyrider.business.core

import akka.actor.{Props, Actor}
import akka.event.LoggingReceive
import easyrider.CommandId
import easyrider.Repository.{Version, Upload, StartUpload}

class RepositoryStorage(uploadFactory: (CommandId, Version) => Props) extends Actor {
  override def receive = LoggingReceive {
    case StartUpload(commandId, version) =>
      val upload = context.actorOf(uploadFactory(commandId, version))
      sender() ! Upload(upload)
  }
}

object RepositoryStorage {
  def apply(uploadFactory: (CommandId, Version) => Props) = Props(classOf[RepositoryStorage], uploadFactory)
}
