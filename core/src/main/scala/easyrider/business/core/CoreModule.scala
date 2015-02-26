package easyrider.business.core

import java.io.File
import java.net.URL

import akka.actor.{ActorRef, ActorSystem}
import easyrider.business.PluginHolder
import easyrider.business.core.repsoritory.{HttpWorker, Repository}
import easyrider.business.http.UploadHandler
import easyrider.business.ssh._

class CoreModule(easyRiderData: File, easyRiderUrl: URL, actorSystem: ActorSystem) {
  val eventBus = actorSystem.actorOf(EventBus(easyRiderData), "EventBus")
  val downloadFactory = RepositoryDownload(easyRiderData) _
  val repository = actorSystem.actorOf(Repository(eventBus), "Repository")
  val uploadFactory = RepositoryUpload(repository) _
  val sshSessionFactory = SshSession(eventBus) _
  val unixInfrastructure = actorSystem.actorOf(PluginHolder(eventBus, ActorRef.noSender /* TODO: undo this hack */)(new UnixServerInfrastructureFactory().props, "UnixInfrastructure"), "UnixInfrastructure")
  val repositoryStorage = actorSystem.actorOf(RepositoryStorage(easyRiderData, uploadFactory, downloadFactory, eventBus))
  val builtInPackageUpload = BuiltInPackageUpload(unixInfrastructure, repositoryStorage) _
  val infrastructure = actorSystem.actorOf(SshInfrastructureProvider(eventBus, SshNodeAgent(eventBus, easyRiderUrl, unixInfrastructure, builtInPackageUpload)), "SshInfrastructure")
  val applicationManager = actorSystem.actorOf(ApplicationManager(eventBus, infrastructure), "ApplicationManager")
  val releaseFactory = OrchestratedDeployment(eventBus, applicationManager) _
  val orchestrator = actorSystem.actorOf(Orchestrator(releaseFactory), "Orchestrator")
  val releaseManager = actorSystem.actorOf(ReleaseManager(eventBus, orchestrator), "ReleaseManager")
  val authenticator = actorSystem.actorOf(Authenticator(), "Authenticator")
  val apiFactory = ApiActor(eventBus, applicationManager, infrastructure, orchestrator, authenticator, repository) _

  val builtinHttpWorker = actorSystem.actorOf(HttpWorker(() => UploadHandler(apiFactory, repositoryStorage)))
}
