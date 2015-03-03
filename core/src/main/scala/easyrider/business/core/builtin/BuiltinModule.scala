package easyrider.business.core.builtin

import java.io.File
import java.net.URL

import akka.actor.{ActorRef, ActorSystem}
import easyrider.business.http.UploadHandler
import easyrider.business.ssh.{BuiltInPackageUpload, ContainerAgent, Containers}

class BuiltinModule(easyRiderData: File, easyRiderUrl: URL, actorSystem: ActorSystem, repository: ActorRef,
                    eventBus: ActorRef, unixInfrastructure: ActorRef) {
  val downloadFactory = RepositoryDownload(easyRiderData) _
  val uploadFactory = RepositoryUpload(repository) _
  val repositoryStorage = actorSystem.actorOf(RepositoryStorage(easyRiderData, uploadFactory, downloadFactory, eventBus))
  val builtInPackageUpload = BuiltInPackageUpload(unixInfrastructure, repositoryStorage) _
  val containers = actorSystem.actorOf(Containers(eventBus, ContainerAgent(eventBus, easyRiderUrl, unixInfrastructure, builtInPackageUpload)), "Containers")
  //val builtinHttpWorker = actorSystem.actorOf(HttpWorker(() => UploadHandler(repositoryStorage)))
}
