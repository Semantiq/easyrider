package easyrider.business.core

import java.io.File
import java.net.URL

import akka.actor.ActorSystem
import easyrider.business.core.repsoritory.Repository

class CoreModule(easyRiderData: File, easyRiderUrl: URL, actorSystem: ActorSystem) {
  val eventBus = actorSystem.actorOf(EventBus(easyRiderData), "EventBus")
  val executionMonitor = actorSystem.actorOf(CommandExecutionMonitor(eventBus), "CommandExecutionMonitor")
  val repository = actorSystem.actorOf(Repository(eventBus), "Repository")
  val containerPluginManager = actorSystem.actorOf(ContainerPluginManager())
  val nodeManager = actorSystem.actorOf(NodeManager(eventBus))

  val applicationManager = actorSystem.actorOf(ApplicationManager(eventBus, containerPluginManager), "ApplicationManager")
  val releaseFactory = OrchestratedDeployment(eventBus, applicationManager) _
  val orchestrator = actorSystem.actorOf(Orchestrator(releaseFactory), "Orchestrator")
  val releaseManager = actorSystem.actorOf(ReleaseManager(eventBus, orchestrator), "ReleaseManager")
  val authenticator = actorSystem.actorOf(Authenticator(), "Authenticator")
  val apiFactory = ApiActor(eventBus, applicationManager, containerPluginManager, orchestrator, authenticator, repository, nodeManager) _
}
