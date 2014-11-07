package easyrider.business.core

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.Applications.{ContainerId, ContainerConfiguration, ContainerConfigurationUpdatedEvent}
import easyrider.Events.{Subscribe, Subscribed}
import easyrider.Implicits._
import easyrider.Infrastructure.{DeploymentState, ContainerStateChangedEvent, DeployVersion, VersionDeploymentProgressEvent}
import easyrider.Orchestrator.ReleaseVersionToStage
import easyrider.{CommandDetails, CommandId, TraceMode}

class OrchestratedDeployment(eventBus: ActorRef, commandCenter: ActorRef, command: ReleaseVersionToStage) extends Actor with ActorLogging {
  override def preStart() = {
    eventBus ! Subscribe(CommandDetails(CommandId.generate(), TraceMode()), "orchestrator-containerconfiguration-" + command.commandDetails.commandId.id, classOf[ContainerConfigurationUpdatedEvent], command.stageId.eventKey)
    eventBus ! Subscribe(CommandDetails(CommandId.generate(), TraceMode()), "orchestrator-versiondeployment-" + command.commandDetails.commandId.id, classOf[VersionDeploymentProgressEvent], command.stageId.eventKey)
    eventBus ! Subscribe(CommandDetails(CommandId.generate(), TraceMode()), "orchestrator-containerstate-" + command.commandDetails.commandId.id, classOf[ContainerStateChangedEvent], command.stageId.eventKey)
  }

  def receive = initializing(None, None, None)

  def initializing(containers: Option[Seq[ContainerConfigurationUpdatedEvent]],
                   deployments: Option[Seq[VersionDeploymentProgressEvent]],
                   containerStates: Option[Seq[ContainerStateChangedEvent]]) = LoggingReceive {
    case Subscribed(_, _, eventType, snapshot: Seq[ContainerConfigurationUpdatedEvent]) if eventType matches classOf[ContainerConfigurationUpdatedEvent] =>
      log.info("Container configuration subscription")
      tryBecomeDeploying(Some(snapshot), deployments, containerStates)
    case Subscribed(_, _, eventType, snapshot: Seq[VersionDeploymentProgressEvent]) if eventType matches classOf[VersionDeploymentProgressEvent] =>
      log.info("Deployment progress subscription")
      tryBecomeDeploying(containers, Some(snapshot), containerStates)
    case Subscribed(_, _, eventType, snapshot: Seq[ContainerStateChangedEvent]) if eventType matches classOf[ContainerStateChangedEvent] =>
      log.info("Container state subscription")
      tryBecomeDeploying(containers, deployments, Some(snapshot))
  }

  def tryBecomeDeploying(containers: Option[Seq[ContainerConfigurationUpdatedEvent]],
                         deployments: Option[Seq[VersionDeploymentProgressEvent]],
                         containerStates: Option[Seq[ContainerStateChangedEvent]]) {
    if (containers.isDefined && deployments.isDefined && containerStates.isDefined) {
      becomeDeploying(containers.get.map(_.container))
    } else {
      context.become(initializing(containers, deployments, containerStates))
    }
  }
  def becomeDeploying(containers: Seq[ContainerConfiguration]) = {
    log.info("Initiating deployment")
    for (container <- containers) {
      commandCenter ! DeployVersion(CommandDetails(CommandId.generate(), TraceMode()), container.id, command.version)
    }
    context.become(deploying(containers, Map()))
  }
  def deploying(containers: Seq[ContainerConfiguration], deployments: Map[ContainerId, DeploymentState]) = LoggingReceive {
    case deployment: VersionDeploymentProgressEvent =>
      println("deployment: " + deployment)
  }
}

object OrchestratedDeployment {
  def apply(eventBus: ActorRef, commandCenter: ActorRef)(command: ReleaseVersionToStage) = Props(classOf[OrchestratedDeployment], eventBus, commandCenter, command)
}
