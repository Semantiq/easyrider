package easyrider.business.core

import java.io.File

import akka.actor._
import akka.event.LoggingReceive
import easyrider.Events.Snapshot
import easyrider.Implicits._
import easyrider._
import org.apache.commons.io.FileUtils
import org.json4s.JsonAST.JArray
import org.json4s.ext.JodaTimeSerializers
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, writePretty}
import org.json4s.{FullTypeHints, JValue}

class EventBus(easyRiderData: File) extends Actor with ActorLogging {
  import easyrider.Events._
  private implicit val formats = Serialization.formats(FullTypeHints(List(classOf[AnyRef]))) ++ JodaTimeSerializers.all ++ EventBusSerializers.serializers

  private case class Subscription(eventType: EventType, eventKey: EventKey, receiver: ActorRef, subscriptionId: String) {
    def matches(event: Event) = eventType.matches(class2eventType(event.getClass)) && eventKey.contains(event.eventDetails.eventKey)
  }
  private case class SnapshotSubscriber(commandId: CommandId, entryType: SnapshotEntryType, subscriber: ActorRef)
  private var snapshots = loadSnapshot()
  private var newSnapshots = loadNewSnapshot()
  private var snapshotSubscribers = Set[SnapshotSubscriber]()
  // TODO: remove after migration
  snapshots.values.flatMap(_.values).foreach(event => processSnapshotUpdate(event))
  private var subscriptions = Set[Subscription]()
  private var eventLog = loadEventLog()

  override def receive: Receive = LoggingReceive {
    case event: Event =>
      if (event.isInstanceOf[SnapshotUpdate[_]] ) {
        processLegacySnapshot(event)
      }
      processSnapshotUpdate(event)
      eventLog +:= event
      save(eventLog)
    case Terminated(subscriber) =>
      subscriptions = subscriptions.filter(s => s.receiver != subscriber)
      snapshotSubscribers = snapshotSubscribers.filter(s => s.subscriber != subscriber)
    case command: EventBusCommand => command match {
      case Subscribe(_, subscriptionId, eventType, eventKey) =>
        val snapshot = snapshots.getOrElse(eventType, Map()).values
          .filter(event => eventKey.contains(event.eventDetails.eventKey))
          .toSeq
        sender() ! Subscribed(command.queryId, subscriptionId, eventType, snapshot)
        subscriptions += Subscription(eventType, eventKey, sender(), subscriptionId)
        context.watch(sender())
      case command @ UnSubscribe(commandId, subscriptionId) =>
        sender() ! UnSubscribed(command.queryId, subscriptionId)
        subscriptions = subscriptions.filter(s => s.subscriptionId != subscriptionId)
      case command @ StartSnapshotSubscription(CommandDetails(commandId, _), entryType) =>
        snapshotSubscribers += SnapshotSubscriber(commandId, entryType, sender())
        sender() ! SnapshotSubscriptionStarted(EventDetails(EventId.generate(), EventKey(), Seq(commandId)), commandId,
          newSnapshots.getOrElse(entryType, Snapshot(entryType, Map())))
        context.watch(sender())
      case command @ StopSnapshotSubscription(_, subscriptionId) =>
        snapshotSubscribers = snapshotSubscribers.filter(s => s.commandId != subscriptionId)
    }
    case GetSnapshot(queryId, eventType) =>
      sender() ! GetSnapshotResponse(queryId, snapshots.getOrElse(eventType, Map()).values.toSeq)
    case GetReplay(queryId, subscriptionIds, since) =>
      val filter = subscriptions.filter(s => subscriptionIds.contains(s.subscriptionId))
      val withinTime = eventLog.dropWhile(e => e.eventDetails.publicationTime isBefore since)
      val matching = withinTime.filter(e => filter.exists(f => f.matches(e)))
      sender() ! GetReplayResponse(queryId, matching)
  }

  def processLegacySnapshot(event: Event): Unit = {
    val current = snapshots.getOrElse(event.getClass, Map())
    val updated = if (event.eventDetails.removal) {
      current - event.eventDetails.eventKey
    } else {
      current.updated(event.eventDetails.eventKey, event)
    }
    snapshots += (class2eventType(event.getClass) -> updated)
    subscriptions
      .filter(s => s.matches(event))
      .foreach(s => s.receiver ! event)
    save(snapshots)
  }

  def processSnapshotUpdate(event: Event) {
    event match {
      case updateEvent: SnapshotUpdate[_] =>
        val update = updateEvent.snapshotUpdate
        val current = newSnapshots.getOrElse(update.entryType, Snapshot(update.entryType, Map())).asInstanceOf[Snapshot[Any]]
        val updated = current updatedWith updateEvent.snapshotUpdate.asInstanceOf[SnapshotUpdateDetails[Any]]
        newSnapshots += (update.entryType -> updated)
        snapshotSubscribers
          .filter(s => s.entryType == update.entryType)
          .foreach {s =>
            s.subscriber ! SnapshotUpdatedEvent(EventDetails(EventId.generate(), EventKey(), Seq(s.commandId)), s.commandId, update)
          }
        saveNew(newSnapshots)
      case _ => // old style event, ignore
    }
  }

  private def snapshotFile = new File(easyRiderData, "snapshot.json")
  private def eventLogFile = new File(easyRiderData, "eventLogFile.json")
  private def newSnapshotFile = new File(easyRiderData, "newSnapshot.json")

  private def save(snapshots: Map[EventType, Map[EventKey, Event]]) = {
    val events = snapshots.values.flatMap(_.values)
    FileUtils.write(snapshotFile, writePretty(events))
  }

  private def saveNew(snapshots: Map[SnapshotEntryType, Snapshot[_]]) = {
    FileUtils.write(newSnapshotFile, EventBus.serializeSnapshots(snapshots))
  }

  private def save(eventLog: Seq[Event]) {
    FileUtils.write(eventLogFile, writePretty(eventLog))
  }

  private def loadNewSnapshot(): Map[SnapshotEntryType, Snapshot[_]] = {
    if (newSnapshotFile.exists()) {
      val string = FileUtils.readFileToString(newSnapshotFile)
      val snapshots = read[Seq[Snapshot[_]]](string)
      snapshots.map(snapshot => snapshot.entryType -> snapshot).toMap
    } else {
      log.info("Could not find snapshot file ({}). Starting with empty snapshots", newSnapshotFile)
      Map()
    }
  }

  private def loadSnapshot(): Map[EventType, Map[EventKey, Event]] = {
    import org.json4s.native.JsonParser.parse

    val deprecatedEvents = Seq(
      "easyrider.business.ssh.SshInfrastructure$RunSshCommandSuccess",
      "easyrider.business.ssh.SshInfrastructure$SftpUploadNextChunk",
      "easyrider.business.ssh.SshInfrastructure$SftpUploadCompleted",
      "easyrider.business.ssh.SshInfrastructure$SftpUpdateFileSuccess"
    )
    if (snapshotFile.exists()) {
      val string = FileUtils.readFileToString(snapshotFile)
      val document: JValue = parse(string)
      val filtered = for (
        value <- document.asInstanceOf[JArray].values if !deprecatedEvents.contains(value.asInstanceOf[Map[String, _]].getOrElse("jsonClass", "?"))
      ) yield {
        value match {
          case map: Map[String, _] if map.get("jsonClass") == Some("easyrider.Infrastructure$ContainerStateChangedEvent") =>
            map.updated("containerId", Map(
              "jsonClass" -> "easyrider.Applications$ContainerId",
              "stageId" -> Map(
                "jsonClass" -> "easyrider.Applications$StageId",
                "applicationId" -> Map(
                  "jsonClass" -> "easyrider.Applications$ApplicationId",
                  "id" -> "_"
                ),
              "id" -> "_"),
            "id" -> "_"))
          case _ => value
        }
      }
      val filteredString = writePretty(filtered)
      FileUtils.write(new File(easyRiderData, "snapshot.migrated.json"), filteredString)
      val events = read[Seq[Event]](string)
      events.groupBy(event => class2eventType(event.getClass)).map {
        case (key, value) => (key, value.groupBy(event => event.eventDetails.eventKey).map {
          case (eventKey, Seq(event)) => (eventKey, event)
        })
      }
    } else {
      log.info("Could not find snapshot file ({}). Starting with empty snapshots", snapshotFile)
      Map()
    }
  }

  private def loadEventLog(): Seq[Event] = {
    if (eventLogFile.exists()) {
      val string = FileUtils.readFileToString(snapshotFile)
      read[Seq[Event]](string)
    } else {
      log.info("Could not find event log file ({}). Starting with empty event log", eventLogFile)
      Seq()
    }
  }
}

object EventBus {
  def apply(easyRiderData: File) = Props(classOf[EventBus], easyRiderData)

  def serializeSnapshots(snapshots: Map[SnapshotEntryType, Snapshot[_]]): String = {
    implicit val formats = Serialization.formats(FullTypeHints(List(classOf[AnyRef]))) ++ JodaTimeSerializers.all ++ EventBusSerializers.serializers
    writePretty(snapshots.values)
  }
}
