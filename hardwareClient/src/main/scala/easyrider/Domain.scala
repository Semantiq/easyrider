package easyrider

trait Cause
case class EventId() extends Cause
case class CommandId() extends Cause

trait Event extends Cause {
  def eventId: EventId
  def causedBy: Seq[Cause]
}

trait InfrastructureEvent
case class NewNodeEvent() extends InfrastructureEvent
case class UnreachableNodeEvent() extends InfrastructureEvent
case class DownNodeEvent() extends InfrastructureEvent

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

trait ApplicationEvent
case class ApplicationCreatedEvent() extends ApplicationEvent
case class ApplicationUpdatedEvent() extends ApplicationEvent
case class ApplicationRemovedEvent() extends ApplicationEvent
case class VersionAvailableEvent() extends ApplicationEvent
case class VersionLabelsAddedEvent() extends ApplicationEvent

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
