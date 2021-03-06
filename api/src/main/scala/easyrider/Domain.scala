package easyrider

import java.util.UUID

import akka.actor.{ActorRef, Props}
import akka.util.ByteString
import easyrider.Applications._
import easyrider.Commands.{CommandFailure, CommandExecution, Failure, Success}
import easyrider.Infrastructure.NodeState
import easyrider.Repository.Version
import org.joda.time.DateTime

import scala.language.implicitConversions

case class ComponentId(id: String)
object ComponentId {
  val core = ComponentId("core")
}

case class EventType(sender: ComponentId, name: String) {
  private val clazz = Class.forName(name)
  def matches(other: EventType) = sender == other.sender && clazz.isAssignableFrom(other.clazz)
}

object Implicits {
  implicit def class2eventType(x: Class[_]): EventType = EventType(ComponentId.core, x.getName)
}

case class CommandDetails(commandId: CommandId = CommandId.generate())

trait Command {
  def commandDetails: CommandDetails
  def failure(message: String) = Failure(EventDetails(EventId.generate()), message, None, commandDetails.commandId)
  def failure(message: String, exception: Throwable) = Failure(EventDetails(EventId.generate()), message, Some(exception), commandDetails.commandId)
}

object Commands {
  case class RegisterProvider(commandClass: Class[_ <: Command])
  trait CommandExecution extends Event {
    def executionOf: CommandId
  }
  trait Success extends CommandExecution {
    def successMessage: String
  }
  trait CommandFailure extends CommandExecution {
    def failureMessage: String
  }
  case class Failure(eventDetails: EventDetails, failureMessage: String, exception: Option[Throwable], executionOf: CommandId = CommandId("?")) extends CommandFailure {
    def isSystemFailure = exception.isDefined
  }
  trait CommandProgress extends CommandExecution {
    def progressOf: CommandId = executionOf
  }
}

sealed trait Query {
  def queryId: QueryId
}

sealed trait Result {
  def queryId: QueryId
}

trait Identifier[Target] {
  def eventKey: EventKey
}

sealed trait Cause
case class EventId(id: String) extends Cause with Identifier[Event] {
  def eventKey = EventKey(id)
}

object EventId {
  def generate() = EventId(UUID.randomUUID().toString)
}

case class CommandId(id: String) extends Cause with Identifier[Command] {
  def eventKey = EventKey(id)
}
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

case class EventDetails(eventId: EventId, publicationTime: DateTime = DateTime.now())

trait Event {
  def eventDetails: EventDetails
}

case class SnapshotEntryType[T](clazz: String)
object SnapshotEntryType {
  def apply[T](clazz: Class[T]) = new SnapshotEntryType[T](clazz.getName)
}
case class SnapshotUpdateDetails[T](entryType: SnapshotEntryType[T], eventKey: EventKey, entry: Option[T])
object SnapshotUpdateDetails {
  def apply[T](eventKey: EventKey, entry: Option[T])(implicit entryType: SnapshotEntryType[T]): SnapshotUpdateDetails[T] = SnapshotUpdateDetails(entryType, eventKey, entry)
}

trait SnapshotUpdate[T] extends Event {
  def snapshotUpdate: SnapshotUpdateDetails[T]
}

case class PackageType(name: String)

case class NodeId(id: String) {
  require(id.matches("""^[a-zA-Z0-9_]+$"""), "Node id can contain only letters and underscores")
  def eventKey = EventKey(id)
}

case class Property(namespace: String, name: String, value: String)

object Infrastructure {
  sealed trait ContainerState
  case object ContainerCreationFailed extends ContainerState
  case object ContainerCreated extends ContainerState
  case class ContainerRunning(version: Version) extends ContainerState
  case class ContainerStopping(version: Version) extends ContainerState
  case class ContainerForceStopping(version: Version) extends ContainerState
  case object ContainerRemoved extends ContainerState
  sealed trait NodeState
  case object CreatingNode extends NodeState
  case object NodeCreated extends NodeState
  sealed trait DeploymentState
  case object DeploymentInProgress extends DeploymentState
  case object DeploymentCompleted extends DeploymentState
  case class DeploymentFailed(reason: String) extends DeploymentState
  case object UnDeploymentInProgress extends DeploymentState
  case object UnDeployed extends DeploymentState
  case class DeploymentInfo(version: Version, state: DeploymentState)

  trait InfrastructureCommand extends Command
  trait ContainerCommand extends InfrastructureCommand {
    def containerId: ContainerId
  }
  case class CreateContainer(commandDetails: CommandDetails, nodeId: NodeId, containerId: ContainerId) extends ContainerCommand
  case class DeployVersion(commandDetails: CommandDetails, containerId: ContainerId, version: Version) extends ContainerCommand
  case class UnDeployVersion(commandDetails: CommandDetails, containerId: ContainerId, version: Version) extends ContainerCommand
  case class DeployConfigurationFile(commandDetails: CommandDetails, containerId: ContainerId, path: String, fileName: String, contents: ByteString) extends ContainerCommand
  case class StartContainer(commandDetails: CommandDetails, containerId: ContainerId, version: Version) extends ContainerCommand
  case class StopContainer(commandDetails: CommandDetails, containerId: ContainerId, immediate: Boolean = false) extends ContainerCommand
  case class RemoveContainer(commandDetails: CommandDetails, containerId: ContainerId, force: Boolean) extends ContainerCommand
  // TODO: move out of API, as this does not need to be publicly available
  case class AddressedContainerCommand(containerType: String, nodeId: NodeId, containerCommand: ContainerCommand)

  case class FindNodes(queryId: QueryId) extends Query
  case class FindNodesResult(sender: ComponentId, queryId: QueryId, nodes: Seq[NodeId]) extends Result

  trait InfrastructureEvent extends Event
  case class ContainerStateChangedEvent(eventDetails: EventDetails, snapshotUpdate: SnapshotUpdateDetails[ContainerState]) extends InfrastructureEvent with SnapshotUpdate[ContainerState]
  case class VersionDeploymentProgressEvent(eventDetails: EventDetails, snapshotUpdate: SnapshotUpdateDetails[DeploymentInfo]) extends InfrastructureEvent with SnapshotUpdate[DeploymentInfo]
  case class NodeUpdatedEvent(eventDetails: EventDetails, snapshotUpdate: SnapshotUpdateDetails[NodeState]) extends InfrastructureEvent with SnapshotUpdate[NodeState]
  case class ContainerCreatedEvent(eventDetails: EventDetails) extends InfrastructureEvent
  case class ContainerCreationError(eventDetails: EventDetails)

  case class DeployConfigurationFileComplete(eventDetails: EventDetails, containerId: ContainerId) extends ContainerEvent
  case class ApplicationStartingEvent(eventDetails: EventDetails, progress: Option[String]) extends Event
  case class ApplicationStartedEvent(eventDetails: EventDetails) extends Event
  case class ApplicationStoppingEvent(eventDetails: EventDetails, progress: Option[String]) extends Event
  case class ApplicationStoppedEvent(eventDetails: EventDetails) extends Event
}

object RemoteAccess {
  trait RemoteAccessCommand extends Command {
    def nodeId: NodeId
  }
  case class RunRemoteCommand(commandDetails: CommandDetails, nodeId: NodeId, command: String) extends RemoteAccessCommand
  case class StartUpload(commandDetails: CommandDetails, nodeId: NodeId, targetFolder: String, targetFileName: String) extends RemoteAccessCommand
  case class UploadChunk(commandDetails: CommandDetails, nodeId: NodeId, uploadId: String, data: BinaryData) extends RemoteAccessCommand
  case class UploadComplete(commandDetails: CommandDetails, nodeId: NodeId, uploadId: String) extends RemoteAccessCommand
  case class UpdateFile(commandDetails: CommandDetails, nodeId: NodeId, path: String, filename: String, content: ByteString) extends RemoteAccessCommand

  trait RemoteAccessEvent extends Event
  case class RunRemoteCommandSuccess(eventDetails: EventDetails, output: Option[String], executionOf: CommandId = CommandId("?"), successMessage: String = "Completed") extends Success with RemoteAccessEvent
  case class UploadNextChunk(eventDetails: EventDetails, uploadId: String, executionOf: CommandId = CommandId("?"), successMessage: String = "Completed") extends Success with RemoteAccessEvent
  case class UploadCompleted(eventDetails: EventDetails, uploadId: String, executionOf: CommandId = CommandId("?"), successMessage: String = "Completed") extends Success with RemoteAccessEvent
  case class UpdateFileSuccess(eventDetails: EventDetails, executionOf: CommandId = CommandId("?"), successMessage: String = "Completed") extends Success with RemoteAccessEvent
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
  trait Authenticate extends Command
  case class AuthenticateUser(commandDetails: CommandDetails, username: String, password: String) extends Authenticate
  case class ReAuthenticateUser(commandDetails: CommandDetails, username: String, signature: String) extends Authenticate
  case class Authentication(eventDetails: EventDetails, username: String, signature: Option[String] = None, executionOf: CommandId, successMessage: String = "Authenticated") extends Success
  case class AuthenticationFailure(eventDetails: EventDetails, executionOf: CommandId, failureMessage: String = "Authentication failed") extends CommandFailure
  case class KeepAlive()
  
  case class CommandSentEvent(eventDetails: EventDetails, command: Command, authentication: Option[Authentication] = None, executionOf: CommandId = CommandId("?")) extends CommandExecution
}

object Events {
  trait EventBusCommand extends Command with Query {
    def queryId = QueryId("query-" + commandDetails.commandId.id)
  }
  case class Subscribe(commandDetails: CommandDetails, subscriptionId: String, eventType: EventType, eventKey: EventKey) extends EventBusCommand
  case class UnSubscribe(commandDetails: CommandDetails, subscriptionId: String) extends EventBusCommand
  case class Subscribed[T](queryId: QueryId, subscriptionId: String, eventType: EventType) extends Result
  case class UnSubscribed(queryId: QueryId, subscriptionId: String) extends Result
  case class GetSnapshot(commandDetails: CommandDetails, entryType: SnapshotEntryType[_]) extends Command {
    def success[T](snapshot: Snapshot[T]) = GetSnapshotResponse(EventDetails(EventId.generate()), snapshot, commandDetails.commandId)
  }
  case class GetSnapshotResponse[T](eventDetails: EventDetails, snapshot: Snapshot[T], executionOf: CommandId,
                                    successMessage: String = "Snapshot delivered") extends Success

  // snapshot based subscriptions
  case class Snapshot[T](entryType: SnapshotEntryType[T], entries: Map[String, T]) {
    def updatedWith(update: SnapshotUpdateDetails[T]): Snapshot[T] = {
      def asString(eventKey: EventKey) = eventKey.key.mkString(":")
      Snapshot(entryType, update.entry match {
        case Some(newValue) => entries + (asString(update.eventKey) -> newValue)
        case None => entries - asString(update.eventKey)
      })
    }
  }
  case class StartSnapshotSubscription[T](commandDetails: CommandDetails, entryType: SnapshotEntryType[T]) extends EventBusCommand
  case class SnapshotSubscriptionStarted[T](eventDetails: EventDetails, executionOf: CommandId, snapshot: Snapshot[T]) extends Event with CommandExecution
  case class SnapshotUpdatedEvent[T](eventDetails: EventDetails, executionOf: CommandId, update: SnapshotUpdateDetails[T]) extends Event with CommandExecution
  case class StopSnapshotSubscription(commandDetails: CommandDetails, subscriptionId: CommandId) extends EventBusCommand
}

object Repository {
  case class Label(name: String, cause: Cause, addedTime: DateTime = DateTime.now())
  case class Version(applicationId: ApplicationId, number: String) {
    def eventKey = EventKey(applicationId.id, number)
  }
  case class VersionMetadata(version: Version, labels: Seq[Label], packageType: PackageType = PackageType("builtin"))
  trait RepositoryEvent extends Event
  case class VersionAvailableEvent(eventDetails: EventDetails, version: Version, snapshotUpdate: SnapshotUpdateDetails[VersionMetadata]) extends RepositoryEvent with SnapshotUpdate[VersionMetadata]

  case class VersionUploadProgressEvent()
  case class StartUpload(commandDetails: CommandDetails, version: Version) extends Command
  case class StartDownload(version: Version)
  case class Upload(upload: ActorRef)
  case class UploadChunk(bytes: ByteString) {
    override def toString = s"UploadChunk(<${bytes.length} bytes>)"
  }
  case object Ack
  case class UploadCompleted()

  trait RepositoryCommand extends Command
  case class AddLabel(commandDetails: CommandDetails, version: Version, name: String) extends RepositoryCommand
  case class DeleteVersion(commandDetails: CommandDetails, version: Version) extends RepositoryCommand

  /**
   * Sent by a plugin, to refer a version of an application in the repository.
   */
  case class NotifyNewVersion(commandDetails: CommandDetails, versionMetadata: VersionMetadata) extends RepositoryCommand
}

case class VersionRecommendedEvent()

object Applications {
  case class EffectiveConfiguration(entries: Map[String, String])

  case class ApplicationId(id: String) extends Identifier[Application] {
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
  object ContainerId {
    def fromEventKey(eventKey: EventKey) = {
      val Seq(applicationId, stageId, containerId) = eventKey.key
      ContainerId(StageId(ApplicationId(applicationId), stageId), containerId)
    }
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
  case class ApplicationUpdatedEvent(eventDetails: EventDetails, executionOf: CommandId, snapshotUpdate: SnapshotUpdateDetails[Application], successMessage: String = "Completed") extends ApplicationEvent with SnapshotUpdate[Application] with Success
  case class EffectiveConfigurationChanged(eventDetails: EventDetails, containerId: ContainerId, effectiveConfiguration: EffectiveConfiguration) extends ApplicationEvent

  trait StageEvent extends Event
  case class StageUpdatedEvent(eventDetails: EventDetails, stage: Stage, snapshotUpdate: SnapshotUpdateDetails[Stage], executionOf: CommandId, successMessage: String = "Completed") extends StageEvent with SnapshotUpdate[Stage] with Success

  trait ContainerEvent extends Event
  case class ContainerConfigurationUpdatedEvent(eventDetails: EventDetails, container: ContainerConfiguration, snapshotUpdate: SnapshotUpdateDetails[ContainerConfiguration], executionOf: CommandId, successMessage: String = "Completed") extends ContainerEvent with SnapshotUpdate[ContainerConfiguration] with Success
  case class ContainerConfigurationRemoved(eventDetails: EventDetails, snapshotUpdate: SnapshotUpdateDetails[ContainerConfiguration]) extends ContainerEvent with SnapshotUpdate[ContainerConfiguration]
}

object Configuration {
  case class RenderedConfiguration(path: String, content: ByteString)
}

trait PluginFactory {
  def props: Props
  def httpHandler(plugin: ActorRef): Option[Props] = None
}

object Plugins {
  case class RegisterContainerPlugin(commandDetails: CommandDetails, name: String) extends Command
  case class RegisterNodeManagementPlugin(commandDetails: CommandDetails, name: String) extends Command
  case class NotifyNodeStatus(commandDetails: CommandDetails, nodeId: NodeId, nodeStatus: NodeState) extends Command
}

case class NodeConfiguration(id: NodeId, nodeType: String, properties: Seq[Property]) {
  def apply(propertyName: String): Option[String] = properties.find(p => p.name == propertyName).map(p => p.value)
}

object Nodes {
  trait NodeManagementCommand extends Command
  case class CreateNode(commandDetails: CommandDetails, nodeConfiguration: NodeConfiguration) extends NodeManagementCommand
  case class UpdateNode(commandDetails: CommandDetails, nodeConfiguration: NodeConfiguration) extends NodeManagementCommand
  case class RemoveNode(commandDetails: CommandDetails, nodeId: NodeId, keepData: Boolean = true) extends NodeManagementCommand

  case class NodeConfigurationUpdatedEvent(eventDetails: EventDetails, snapshotUpdate: SnapshotUpdateDetails[NodeConfiguration]) extends Event with SnapshotUpdate[NodeConfiguration]
}

/*
TODO: use for inspiration in next phases

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
*/