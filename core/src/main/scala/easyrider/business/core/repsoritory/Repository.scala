package easyrider.business.core.repsoritory

import akka.actor.{ActorRef, Props, Actor, ActorLogging}
import akka.event.LoggingReceive
import easyrider.Repository._
import easyrider._

class Repository(eventBus: ActorRef) extends Actor with ActorLogging {
  override def receive = LoggingReceive {
    case NotifyNewVersion(commandDetails, versionMetadata) =>
      eventBus ! VersionAvailableEvent(
        EventDetails(EventId.generate()),
        versionMetadata.version,
        SnapshotUpdateDetails(SnapshotEntryType(classOf[VersionMetadata]), versionMetadata.version.eventKey, Some(versionMetadata)))
    case AddLabel(commandDetails, version, name) =>
      Label(name, commandDetails.commandId)
      // TODO: implement
    case DeleteVersion(commandDetails, version) =>
      eventBus ! VersionAvailableEvent(
        EventDetails(EventId.generate()),
        version,
        SnapshotUpdateDetails(SnapshotEntryType(classOf[VersionMetadata]), version.eventKey, None))

  }
}

object Repository {
  def apply(eventBus: ActorRef) = Props(classOf[Repository], eventBus)
}
