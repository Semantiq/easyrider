package easyrider.builtin

import java.io.File
import java.net.URL

import akka.actor.{ActorRef, ActorSystem}

class BuiltinModule(easyRiderData: File, easyRiderUrl: URL, actorSystem: ActorSystem, repository: ActorRef,
                    eventBus: ActorRef, unixInfrastructure: ActorRef) {
  val downloadFactory = RepositoryDownload(easyRiderData) _
  val uploadFactory = RepositoryUpload(repository) _
  val repositoryStorage = actorSystem.actorOf(RepositoryStorage(easyRiderData, uploadFactory, downloadFactory, eventBus), "RepositoryStorage")
  val builtInPackageUpload = BuiltInPackageUpload(unixInfrastructure, repositoryStorage) _
  val containers = actorSystem.actorOf(Containers(eventBus, ContainerAgent(eventBus, easyRiderUrl, unixInfrastructure, builtInPackageUpload)), "Containers")
}
