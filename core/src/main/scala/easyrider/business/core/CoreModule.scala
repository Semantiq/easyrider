package easyrider.business.core

import java.io.File
import java.net.URL

import akka.actor.{ActorRef, ActorSystem}
import easyrider.business.PluginHolder
import easyrider.business.ssh._

class CoreModule(easyRiderData: File, easyRiderUrl: URL, actorSystem: ActorSystem) {
  val eventBus = actorSystem.actorOf(EventBus(easyRiderData), "EventBus")
  val uploadFactory = RepositoryUpload(eventBus) _
  val downloadFactory = RepositoryDownload(easyRiderData) _
  val repositoryStorage = actorSystem.actorOf(RepositoryStorage(easyRiderData, uploadFactory, downloadFactory))
  val sshSessionFactory = SshSession(eventBus, repositoryStorage) _
  val unixInfrastructure = actorSystem.actorOf(PluginHolder(eventBus, ActorRef.noSender /* TODO: undo this hack */)(new UnixServerInfrastructureFactory().props, "UnixInfrastructure"), "UnixInfrastructure")
  val builtInPackageUpload = BuiltInPackageUpload(unixInfrastructure, repositoryStorage) _
  val infrastructure = actorSystem.actorOf(SshInfrastructureProvider(eventBus, SshNodeAgent(eventBus, easyRiderUrl, unixInfrastructure, builtInPackageUpload)), "SshInfrastructure")
  val applicationManager = actorSystem.actorOf(ApplicationManager(eventBus, infrastructure), "ApplicationManager")
  val componentManager = actorSystem.actorOf(ComponentManager(), "ComponentManager")
  val releaseFactory = OrchestratedDeployment(eventBus, applicationManager) _
  val orchestrator = actorSystem.actorOf(Orchestrator(releaseFactory), "Orchestrator")
  val releaseManager = actorSystem.actorOf(ReleaseManager(eventBus, orchestrator), "ReleaseManager")
  val authenticator = actorSystem.actorOf(Authenticator(), "Authenticator")
  val apiFactory = ApiActor(eventBus, applicationManager, componentManager, infrastructure, repositoryStorage, orchestrator, authenticator) _
}
