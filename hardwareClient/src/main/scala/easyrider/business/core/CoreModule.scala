package easyrider.business.core

import java.io.File

import akka.actor.ActorSystem

class CoreModule(easyriderData: File, actorSystem: ActorSystem) {
  val eventBus = actorSystem.actorOf(EventBus(easyriderData), "EventBus")
  val uploadFactory = RepositoryUpload(eventBus) _
  val downloadFactory = RepositoryDownload(easyriderData) _
  val repositoryStorage = actorSystem.actorOf(RepositoryStorage(easyriderData, uploadFactory, downloadFactory))
  val deployerFactory = SshNodeDeployer(eventBus, repositoryStorage) _
  val infrastructure = actorSystem.actorOf(SshInfrastructure(eventBus, SshNodeAgent(eventBus, deployerFactory)), "SshInfrastructure")
  val applicationManager = actorSystem.actorOf(ApplicationManager(eventBus, infrastructure), "ApplicationManager")
  val componentManager = actorSystem.actorOf(ComponentManager(), "ComponentManager")
  val apiFactory = ApiActor(eventBus, applicationManager, componentManager, infrastructure, repositoryStorage) _
}
