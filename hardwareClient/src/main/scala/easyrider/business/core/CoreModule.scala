package easyrider.business.core

import java.io.File

import akka.actor.ActorSystem

class CoreModule(easyriderData: File, actorSystem: ActorSystem) {
  val eventBus = actorSystem.actorOf(EventBus(), "EventBus")
  val applicationManager = actorSystem.actorOf(ApplicationManager(eventBus), "ApplicationManager")
  val infrastructure = actorSystem.actorOf(SshInfrastructure(SshNodeAgent(eventBus)), "SshInfrastructure")
  val componentManager = actorSystem.actorOf(ComponentManager(), "ComponentManager")
  val uploadFactory = RepositoryUpload(eventBus) _
  val repositoryStorage = actorSystem.actorOf(RepositoryStorage(easyriderData, uploadFactory))
  val apiFactory = ApiActor(eventBus, applicationManager, componentManager, infrastructure, repositoryStorage) _
}
