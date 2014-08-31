package easyrider.business.core

import java.io.File

import akka.actor.ActorSystem

class CoreModule(easyriderData: File, actorSystem: ActorSystem) {
  val eventBus = actorSystem.actorOf(EventBus(easyriderData), "EventBus")
  val infrastructure = actorSystem.actorOf(SshInfrastructure(eventBus, SshNodeAgent(eventBus)), "SshInfrastructure")
  val applicationManager = actorSystem.actorOf(ApplicationManager(eventBus, infrastructure), "ApplicationManager")
  val componentManager = actorSystem.actorOf(ComponentManager(), "ComponentManager")
  val uploadFactory = RepositoryUpload(eventBus) _
  val repositoryStorage = actorSystem.actorOf(RepositoryStorage(easyriderData, uploadFactory))
  val apiFactory = ApiActor(eventBus, applicationManager, componentManager, infrastructure, repositoryStorage) _
}
