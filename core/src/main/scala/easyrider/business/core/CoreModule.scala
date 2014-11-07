package easyrider.business.core

import java.io.File
import java.net.URL

import akka.actor.ActorSystem
import easyrider.business.ssh.{SshSession, SshNodeAgent, SshInfrastructureProvider}

class CoreModule(easyRiderData: File, easyRiderUrl: URL, actorSystem: ActorSystem) {
  val eventBus = actorSystem.actorOf(EventBus(easyRiderData), "EventBus")
  val uploadFactory = RepositoryUpload(eventBus) _
  val downloadFactory = RepositoryDownload(easyRiderData) _
  val repositoryStorage = actorSystem.actorOf(RepositoryStorage(easyRiderData, uploadFactory, downloadFactory))
  val sshSessionFactory = SshSession(eventBus, repositoryStorage) _
  val infrastructure = actorSystem.actorOf(SshInfrastructureProvider(eventBus, SshNodeAgent(eventBus, easyRiderUrl, sshSessionFactory)), "SshInfrastructure")
  val applicationManager = actorSystem.actorOf(ApplicationManager(eventBus, infrastructure), "ApplicationManager")
  val componentManager = actorSystem.actorOf(ComponentManager(), "ComponentManager")
  val commandCenter = actorSystem.actorOf(CommandCenter(eventBus), "CommandCenter")
  val releaseFactory = OrchestratedDeployment(eventBus, commandCenter) _
  val orchestrator = actorSystem.actorOf(Orchestrator(releaseFactory), "Orchestrator")
  val apiFactory = ApiActor(eventBus, applicationManager, componentManager, infrastructure, repositoryStorage, orchestrator) _
}
