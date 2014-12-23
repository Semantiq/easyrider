package easyrider

import java.util.UUID

import akka.actor.{Props, ActorRef}
import akka.util.ByteString
import easyrider.Applications._
import easyrider.Commands.{CommandExecution, Failure}
import easyrider.Infrastructure.NodeId
import easyrider.Repository.Version
import org.joda.time.DateTime

case class ComponentId(id: String)
object ComponentId {
  val core = ComponentId("core")
}

case class EventType(sender: ComponentId, name: String) {
  private val clazz = Class.forName(name)
  def matches(other: EventType) = sender == other.sender && clazz.isAssignableFrom(other.clazz)
}

object Implicits {
  implicit def class2eventType(x: Class[_]) = EventType(ComponentId.core, x.getName)
}

case class TraceMode(progress: Boolean = false, confirmation: Boolean = true, consequences: Boolean = false)
case class CommandDetails(commandId: CommandId = CommandId.generate(), trace: TraceMode = TraceMode())

trait Command {
  def commandDetails: CommandDetails
  def failure(message: String) = Failure(EventDetails(EventId.generate(), EventKey(commandDetails.commandId.id), Seq(commandDetails.commandId)), message, None)
  def failure(message: String, exception: Throwable) = Failure(EventDetails(EventId.generate(), EventKey(commandDetails.commandId.id), Seq(commandDetails.commandId)), message, Some(exception))
}

object Commands {
  case class RegisterProvider(commandClass: Class[_ <: Command])
  trait CommandExecution extends Event
  trait Success extends CommandExecution
  case class Failure(eventDetails: EventDetails, message: String, exception: Option[Throwable]) extends CommandExecution {
    def isSystemFailure = exception.isDefined
  }
  trait CommandProgress extends CommandExecution
}

object Components {
  case class ExtensionId(id: String)
  case class ConsoleExtension(componentId: ComponentId, extensionId: ExtensionId) {
    def eventKey = EventKey(componentId.id, extensionId.id)
  }

  case class ComponentCommand(commandDetails: CommandDetails, componentId: ComponentId, payload: Map[String, String]) extends Command
  case class ComponentEvent(eventDetails: EventDetails, payload: Map[String, String]) extends Event
  case class ConsoleExtensionAvailableEvent(eventDetails: EventDetails, extension: ConsoleExtension) extends Event
}

sealed trait Query {
  def queryId: QueryId
}

sealed trait Result {
  def queryId: QueryId
}

sealed trait Cause
case class EventId(id: String) extends Cause

object EventId {
  def generate() = EventId(UUID.randomUUID().toString)
}

case class CommandId(id: String) extends Cause
object CommandId {
  def generate() = CommandId(UUID.randomUUID().toString)
}

case class QueryId(id: String)
object QueryId {
  def generate() = QueryId(UUID.randomUUID().toString)
}
case class EventKey(key: String*) {
  def contains(other: EventKey): Boolean = {
    other.key.take(key.size) == key
  }
  def append(extraKey: String) = EventKey(key :+ extraKey :_*)
}

case class EventDetails(eventId: EventId, eventKey: EventKey, causedBy: Seq[Cause], removal: Boolean = false,
                         publicationTime: DateTime = DateTime.now())

trait Event {
  def eventDetails: EventDetails
}

object Infrastructure {
  case class NodeId(id: String) {
    require(id.matches("""^[a-zA-Z0-9_]+$"""), "Node id can contain only letters and underscores")
  }
  sealed trait ContainerState
  case object ContainerCreationFailed extends ContainerState
  case object ContainerCreated extends ContainerState
  case class ContainerRunning(version: Version) extends ContainerState
  case class ContainerStopping(version: Version) extends ContainerState
  case class ContainerForceStopping(version: Version) extends ContainerState
  sealed trait NodeState
  case object CreatingNode extends NodeState
  case object NodeCreated extends NodeState
  sealed trait DeploymentState
  case object DeploymentInProgress extends DeploymentState
  case object DeploymentCompleted extends DeploymentState
  case class DeploymentFailed(reason: String) extends DeploymentState

  trait InfrastructureCommand extends Command
  case class CreateContainer(commandDetails: CommandDetails, nodeId: NodeId, containerId: ContainerId) extends InfrastructureCommand
  trait ContainerCommand extends InfrastructureCommand {
    def containerId: ContainerId
  }
  case class DeployVersion(commandDetails: CommandDetails, containerId: ContainerId, version: Version) extends ContainerCommand
  case class DeployConfigurationFile(commandDetails: CommandDetails, containerId: ContainerId, path: String, fileName: String, contents: ByteString) extends ContainerCommand
  case class StartContainer(commandDetails: CommandDetails, containerId: ContainerId, version: Version) extends ContainerCommand
  case class StopContainer(commandDetails: CommandDetails, containerId: ContainerId, immediate: Boolean = false) extends ContainerCommand
  // TODO: move out of API, as this does not need to be publicly available
  case class AddressedContainerCommand(nodeId: NodeId, containerCommand: ContainerCommand)

  case class FindNodes(queryId: QueryId) extends Query
  case class FindNodesResult(sender: ComponentId, queryId: QueryId, nodes: Seq[NodeId]) extends Result

  trait InfrastructureEvent extends Event
  case class ContainerStateChangedEvent(eventDetails: EventDetails, state: ContainerState) extends InfrastructureEvent
  case class VersionDeploymentProgressEvent(eventDetails: EventDetails, version: Version, state: DeploymentState) extends InfrastructureEvent
  case class NodeUpdatedEvent(eventDetails: EventDetails, nodeId: NodeId, state: NodeState) extends InfrastructureEvent
  case class ContainerCreatedEvent(eventDetails: EventDetails) extends InfrastructureEvent
  case class ContainerCreationError(eventDetails: EventDetails)

  case class DeployConfigurationFileComplete(eventDetails: EventDetails, containerId: ContainerId) extends ContainerEvent
  case class ApplicationStartingEvent(eventDetails: EventDetails, progress: Option[String]) extends Event
  case class ApplicationStartedEvent(eventDetails: EventDetails) extends Event
  case class ApplicationStoppingEvent(eventDetails: EventDetails, progress: Option[String]) extends Event
  case class ApplicationStoppedEvent(eventDetails: EventDetails) extends Event
}

object Orchestrator {
  sealed trait ReleaseStatus
  case class ReleaseInProgress(comment: String, version: Version) extends ReleaseStatus
  case class ReleaseSuccessful(version: Version) extends ReleaseStatus

  trait OrchestrationCommand extends Command
  case class ReleaseVersionToStage(commandDetails: CommandDetails, stageId: StageId, version: Version) extends OrchestrationCommand

  trait OrchestratorEvent extends Event
  case class ReleaseEvent(eventDetails: EventDetails, releaseStatus: ReleaseStatus) extends OrchestratorEvent
}

object Api {
  trait Authenticate
  case class AuthenticateUser() extends Authenticate
  case class AuthenticateComponent(componentId: ComponentId) extends Authenticate
  case class Authentication()
  
  case class CommandSentEvent(eventDetails: EventDetails, command: Command, authentication: Option[Authentication] = None) extends CommandExecution
}

object Events {
  trait EventBusCommand extends Command with Query {
    def queryId = QueryId("query-" + commandDetails.commandId.id)
  }
  case class Subscribe(commandDetails: CommandDetails, subscriptionId: String, eventType: EventType, eventKey: EventKey) extends EventBusCommand
  case class UnSubscribe(commandDetails: CommandDetails, subscriptionId: String) extends EventBusCommand
  case class Subscribed[T](queryId: QueryId, subscriptionId: String, eventType: EventType, snapshot: Seq[T]) extends Result
  case class UnSubscribed(queryId: QueryId, subscriptionId: String) extends Result
  case class GetSnapshot(queryId: QueryId, eventType: EventType) extends Query
  case class GetSnapshotResponse[T](queryId: QueryId, snapshot: Seq[T]) extends Result
  case class GetReplay(queryId: QueryId, subscriptions: Seq[String], since: DateTime) extends Query
  case class GetReplayResponse(queryId: QueryId, events: Seq[Event]) extends Result

  // new flavour of subscriptions
  case class SubscribeToCommandTrail(commandDetails: CommandDetails, commandId: CommandId, trace: Seq[Class[_ <: Event]]) extends EventBusCommand
  case class EventDelivered(eventDetails: EventDetails, event: Event) extends Event
  case class EventDeliveryComplete(eventDetails: EventDetails) extends Event
}

object Repository {
  case class Label(name: String, cause: Cause, addedTime: DateTime = DateTime.now())
  case class Version(applicationId: ApplicationId, number: String) {
    def eventKey = EventKey(applicationId.id, number)
  }
  trait RepositoryEvent extends Event
  case class VersionUploadProgressEvent()
  case class VersionAvailableEvent(eventDetails: EventDetails, version: Version) extends RepositoryEvent
  case class VersionLabelsAddedEvent(eventDetails: EventDetails, version: Version, newLabels: Seq[Label],
                                     labels: Seq[Label]) extends RepositoryEvent

  case class StartUpload(commandDetails: CommandDetails, version: Version) extends Command
  case class StartDownload(version: Version)
  case class Upload(upload: ActorRef)
  case class UploadChunk(bytes: ByteString)
  case object Ack
  case class UploadCompleted()
}

case class VersionRecommendedEvent()

object Applications {
  case class Property(namespace: String, name: String, value: String)
  case class EffectiveConfiguration(entries: Map[String, String])

  case class ApplicationId(id: String) {
    require(id.matches("""^[a-zA-Z0-9_]+$"""), "Application id can contain only letters and underscores")
    def eventKey = EventKey(id)
  }
  case class Application(id: ApplicationId, properties: Seq[Property])
  case class StageId(applicationId: ApplicationId, id: String) {
    require(id.matches("""^[a-zA-Z0-9_]+$"""), "Stage id can contain only letters and underscores")
    def eventKey = EventKey(applicationId.id, id)
  }
  case class Stage(id: StageId, properties: Seq[Property])
  case class ContainerId(stageId: StageId, id: String) {
    require(id.matches("""^[a-zA-Z0-9_]+$"""), "Container id can contain only letters and underscores")
    def eventKey = EventKey(stageId.applicationId.id, stageId.id, id)
    def containerName = stageId.applicationId.id + "-" + stageId.id + "-" + id
  }
  case class ContainerConfiguration(id: ContainerId, nodeId: NodeId, properties: Seq[Property])
  trait ApplicationCommand extends Command
  case class CreateApplication(commandDetails: CommandDetails, application: Application) extends ApplicationCommand
  case class RemoveApplication(commandDetails: CommandDetails, applicationId: ApplicationId) extends ApplicationCommand
  case class UpdateApplication(commandDetails: CommandDetails, application: Application) extends ApplicationCommand
  case class CreateStage(commandDetails: CommandDetails, stage: Stage) extends ApplicationCommand
  case class UpdateStage(commandDetails: CommandDetails, stage: Stage) extends ApplicationCommand
  case class RemoveStage(commandDetails: CommandDetails, stageId: StageId) extends ApplicationCommand
  case class CreateContainerConfiguration(commandDetails: CommandDetails, container: ContainerConfiguration) extends ApplicationCommand
  case class UpdateContainerConfiguration(commandDetails: CommandDetails, container: ContainerConfiguration) extends ApplicationCommand

  trait ApplicationEvent extends Event
  case class ApplicationUpdatedEvent(eventDetails: EventDetails, application: Application) extends ApplicationEvent
  case class EffectiveConfigurationChanged(eventDetails: EventDetails, containerId: ContainerId, effectiveConfiguration: EffectiveConfiguration) extends ApplicationEvent

  trait StageEvent extends Event
  case class StageUpdatedEvent(eventDetails: EventDetails, stage: Stage) extends StageEvent

  trait ContainerEvent extends Event
  case class ContainerConfigurationUpdatedEvent(eventDetails: EventDetails, container: ContainerConfiguration) extends ContainerEvent
}

object Configuration {
  case class RenderedConfiguration(path: String, content: ByteString)
}

trait PluginFactory {
  def props: Props
}

trait ResourceEvent
case class ResourceCreatedEvent() extends ResourceEvent
case class ResourceUpdatedEvent() extends ResourceEvent
case class ResourceRemovedEvent() extends ResourceEvent
case class ResourceSubscriptionAdded() extends ResourceEvent
case class ResourceSubscriptionRemoved() extends ResourceEvent

trait CapacityEvent
case class MeasurementEvent() extends CapacityEvent
case class CapacityUpdatedEvent() extends CapacityEvent

case class CommandExplainedResponse()
