package easyrider.business.core

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.Applications.{ContainerId, ContainerConfiguration, ContainerConfigurationUpdatedEvent}
import easyrider.Events.{Subscribe, Subscribed}
import easyrider.Implicits._
import easyrider.Infrastructure._
import easyrider.Orchestrator.{ReleaseEvent, ReleaseSuccessful, ReleaseVersionToStage}
import easyrider._

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
    val deploymentCommands = for (container <- containers) yield {
      val commandId = CommandId.generate()
      commandCenter ! DeployVersion(CommandDetails(commandId, TraceMode()), container.id, command.version)
      commandId
    }
    context.become(deploying(containers, deploymentCommands.toSet))
  }
  def deploying(containers: Seq[ContainerConfiguration], deployments: Set[CommandId]) = LoggingReceive {
    case deployment: VersionDeploymentProgressEvent =>
      tryBecomeReleasing(containers, deployments - deployment.eventDetails.causedBy.head.asInstanceOf[CommandId])
      println("deployment: " + deployment)
  }
  def tryBecomeReleasing(containers: Seq[ContainerConfiguration], deployments: Set[CommandId]) {
    if (deployments.isEmpty) {
      becomeReleasing(containers)
    } else {
      context.become(deploying(containers, deployments))
    }
  }
  def becomeReleasing(containers: Seq[ContainerConfiguration]) {
    if (containers.nonEmpty) {
      commandCenter ! StartContainer(CommandDetails(CommandId.generate(), TraceMode()), containers.head.id, command.version)
      context.become(waitingForContainerStart(containers.head.id, containers.tail))
    } else {
      eventBus ! ReleaseEvent(EventDetails(EventId.generate(), command.stageId.eventKey, Seq(command.commandDetails.commandId)), ReleaseSuccessful(command.version))
      context.stop(self)
    }
  }
  def waitingForContainerStart(container: ContainerId, containers: Seq[ContainerConfiguration]) = LoggingReceive {
    case ContainerStateChangedEvent(eventDetails, ContainerRunning(version)) =>
      becomeReleasing(containers)
  }
}

object OrchestratedDeployment {
  def apply(eventBus: ActorRef, commandCenter: ActorRef)(command: ReleaseVersionToStage) = Props(classOf[OrchestratedDeployment], eventBus, commandCenter, command)
}
