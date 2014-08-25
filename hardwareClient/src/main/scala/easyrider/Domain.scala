package easyrider

import java.util.UUID

import easyrider.Applications.ContainerId
import easyrider.Infrastructure.NodeId
import easyrider.business.core.EventBus

sealed trait Target
case class ComponentId(id: String) extends Target
object ComponentId {
  val core = ComponentId("core")
}

case class EventType(sender: ComponentId, name: String)

object Implicits {
  implicit def class2eventType(x: Class[_]) = EventType(ComponentId.core, x.getName)
}

case class Failure(commandId: CommandId, message: String, exception: Option[Exception]) {
  def isSystemFailure = exception.isDefined
}

sealed trait Command {
  def commandId: CommandId
  def failure(message: String) = Failure(commandId, message, None)
  def systemFailure(message: String, exception: Exception) = Failure(commandId, message, Some(exception))
}

object Components {
  case class ExtensionId(id: String)
  case class ConsoleExtension(componentId: ComponentId, extensionId: ExtensionId) {
    def eventKey = EventKey(componentId.id, extensionId.id)
  }

  case class ComponentCommand(commandId: CommandId, componentId: ComponentId, payload: Map[String, String]) extends Command
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
case class EventKey(key: String*) {
  def contains(other: EventKey): Boolean = {
    other.key.take(key.size) == key
  }
}

case class EventDetails(eventId: EventId, eventKey: EventKey, causedBy: Seq[Cause], removal: Boolean = false)

sealed trait Event {
  def eventDetails: EventDetails
}

object Infrastructure {
  case class NodeId(id: String) {
    require(id.matches("""^[a-zA-Z0-9_]+$"""), "Node id can contain only letters and underscores")
  }
  sealed trait ContainerState
  case object CreationFailed extends ContainerState
  case object Created extends ContainerState
  sealed trait NodeState
  case object CreatingNode extends NodeState
  case object NodeCreated extends NodeState

  trait InfrastructureCommand extends Command
  case class CreateContainer(commandId: CommandId, nodeId: NodeId, containerId: ContainerId) extends InfrastructureCommand

  case class FindNodes(queryId: QueryId) extends Query
  case class FindNodesResult(sender: ComponentId, queryId: QueryId, nodes: Seq[NodeId]) extends Result

  trait InfrastructureEvent extends Event
  case class ContainerStateChangedEvent(eventDetails: EventDetails, state: ContainerState) extends InfrastructureEvent
  case class NodeUpdatedEvent(eventDetails: EventDetails, state: NodeState) extends InfrastructureEvent
  case class ContainerCreatedEvent(eventDetails: EventDetails) extends InfrastructureEvent
  case class ContainerCreationError(eventDetails: EventDetails)
}

object SshInfrastructure {
  case class NodeConfiguration(id: NodeId, host: String, port: Int, login: String, password: String)
  trait SshInfrastructureCommand extends Command
  case class CreateNode(commandId: CommandId, nodeConfiguration: NodeConfiguration) extends SshInfrastructureCommand
  case class UpdateNode(commandId: CommandId, nodeConfiguration: NodeConfiguration) extends SshInfrastructureCommand
  case class RemoveNode(commandId: CommandId, nodeId: NodeId, keepData: Boolean = true) extends SshInfrastructureCommand

  case class NodeConfigurationUpdated(eventDetails: EventDetails, nodeConfiguration: NodeConfiguration) extends Event
}

object Api {
  trait Authenticate
  case class AuthenticateUser() extends Authenticate
  case class AuthenticateComponent(componentId: ComponentId) extends Authenticate
  case class Authentication()
  
  case class CommandSentEvent(eventDetails: EventDetails, command: Command, authentication: Authentication) extends Event
}

object Events {
  trait EventBusCommand extends Command with Query {
    def queryId = QueryId("query-" + commandId.id)
  }
  case class Subscribe(commandId: CommandId, subscriptionId: String, eventType: EventType, eventKey: EventKey) extends EventBusCommand
  case class UnSubscribe(commandId: CommandId, subscriptionId: String) extends EventBusCommand
  case class Subscribed[T](queryId: QueryId, subscriptionId: String, eventType: EventType, snapshot: Seq[T]) extends Result
  case class UnSubscribed(queryId: QueryId, subscriptionId: String) extends Result

  val id = ComponentId(classOf[EventBus].getName)
}

case class VersionAvailableEvent(eventDetails: EventDetails)
case class VersionLabelsAddedEvent(eventDetails: EventDetails)

case class VersionRecommendedEvent()
case class ReleaseProgressEvent()
case class ReleaseSuccessful()
case class ReleaseFailed()

object Applications {
  case class Property(namespace: String, name: String, value: String)
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
  case class ContainerConfiguration(id: ContainerId, properties: Seq[Property])
  trait ApplicationCommand extends Command
  case class CreateApplication(commandId: CommandId, application: Application) extends ApplicationCommand
  case class RemoveApplication(commandId: CommandId, applicationId: ApplicationId) extends ApplicationCommand
  case class UpdateApplication(commandId: CommandId, application: Application) extends ApplicationCommand
  case class CreateStage(commandId: CommandId, stage: Stage) extends ApplicationCommand
  case class UpdateStage(commandId: CommandId, stage: Stage) extends ApplicationCommand
  case class RemoveStage(commandId: CommandId, stageId: StageId) extends ApplicationCommand
  case class CreateContainerConfiguration(commandId: CommandId, container: ContainerConfiguration) extends ApplicationCommand
  case class UpdateContainerConfiguration(commandId: CommandId, container: ContainerConfiguration) extends ApplicationCommand

  trait ApplicationEvent extends Event
  case class ApplicationUpdatedEvent(eventDetails: EventDetails, application: Application) extends ApplicationEvent

  trait StageEvent extends Event
  case class StageUpdatedEvent(eventDetails: EventDetails, stage: Stage) extends StageEvent

  trait ContainerEvent extends Event
  case class ContainerConfigurationUpdatedEvent(eventDetails: EventDetails, container: ContainerConfiguration) extends ContainerEvent
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
