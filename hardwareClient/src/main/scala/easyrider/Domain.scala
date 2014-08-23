package easyrider

import java.util.UUID

import easyrider.business.core.EventBus

sealed trait Target
case class PluginTarget(pluginId: ComponentId) extends Target
case class ComponentId(id: String)

case class Failure(commandId: CommandId, message: String, exception: Option[Exception]) {
  def isSystemFailure = exception.isDefined
}

sealed trait Command {
  def commandId: CommandId
  def failure(message: String) = Failure(commandId, message, None)
  def systemFailure(message: String, exception: Exception) = Failure(commandId, message, Some(exception))
}
sealed trait Query {
  def queryId: QueryId
}

sealed trait Result {
  def queryId: QueryId
  def sender: ComponentId
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
  trait InfrastructureCommand extends Command
  case class CreateContainer(commandId: CommandId) extends InfrastructureCommand

  case class FindNodes(queryId: QueryId) extends Query
  case class FindNodesResult(sender: ComponentId, queryId: QueryId) extends Result

  trait InfrastructureEvent extends Event
  case class NodeUpdatedEvent(eventDetails: EventDetails) extends InfrastructureEvent
  case class ContainerCreatedEvent(eventDetails: EventDetails) extends InfrastructureEvent
  case class ContainerCreationError(eventDetails: EventDetails)
}

object Api {
  case class Authenticate()
  case class Authentication()
  
  case class CommandSentEvent(eventDetails: EventDetails, command: Command, authentication: Authentication) extends Event
}

object Events {
  trait EventBusCommand extends Command with Query {
    def queryId = QueryId("query-" + commandId.id)
  }
  case class Subscribe[T <: Event](commandId: CommandId, subscriptionId: String, eventType: Class[T], eventKey: EventKey) extends EventBusCommand
  case class UnSubscribe(commandId: CommandId, subscriptionId: String) extends EventBusCommand
  case class Subscribed[T >: Event](queryId: QueryId, subscriptionId: String, eventType: Class[T], snapshot: Map[EventKey, T]) extends Result {
    def sender = id
  }
  case class UnSubscribed(queryId: QueryId, subscriptionId: String) extends Result {
    def sender = id
  }

  val id = ComponentId(classOf[EventBus].getName)
}

trait ContainerEvent
case class ContainerCreatedEvent() extends ContainerEvent
case class ContainerUpdatedEvent() extends ContainerEvent
case class ContainerDeployedEvent() extends ContainerEvent
case class ContainerStateChangedEvent() extends ContainerEvent

trait StageEvent
case class StageCreatedEvent() extends StageEvent
case class StageUpdatedEvent() extends StageEvent
case class StageRemovedEvent() extends StageEvent
case class VersionRecommendedEvent() extends StageEvent
case class ReleaseProgressEvent() extends StageEvent
case class ReleaseSuccessful() extends StageEvent
case class ReleaseFailed() extends StageEvent

object Applications {
  case class Property(namespace: String, name: String, value: String)
  case class ApplicationId(id: String) {
    require("""^[a-zA-Z0-9_\-]+$""".r.findFirstIn(id).isDefined, "Application name can contain only letters and underscores")
  }
  case class Application(id: ApplicationId, properties: Seq[Property])
  trait ApplicationEvent extends Event
  trait ApplicationCommand extends Command
  case class CreateApplication(commandId: CommandId, application: Application) extends ApplicationCommand
  case class RemoveApplication(commandId: CommandId, applicationId: ApplicationId) extends ApplicationCommand
  
  case class ApplicationUpdatedEvent(eventDetails: EventDetails, application: Application) extends ApplicationEvent
  case class VersionAvailableEvent(eventDetails: EventDetails) extends ApplicationEvent
  case class VersionLabelsAddedEvent(eventDetails: EventDetails) extends ApplicationEvent
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
