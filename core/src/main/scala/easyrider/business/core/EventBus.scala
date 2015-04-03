package easyrider.business.core

import java.io.File

import akka.actor._
import akka.event.LoggingReceive
import easyrider.Implicits._
import easyrider._
import org.apache.commons.io.FileUtils
import org.json4s.FullTypeHints
import org.json4s.ext.JodaTimeSerializers
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, writePretty}

class EventBus(easyRiderData: File) extends Actor with ActorLogging {
  import easyrider.Events._
  private implicit val formats = Serialization.formats(FullTypeHints(List(classOf[AnyRef]))) ++ JodaTimeSerializers.all ++ EventBusSerializers.serializers

  private case class Subscription(eventType: EventType, eventKey: EventKey, receiver: ActorRef, subscriptionId: String) {
    def matches(event: Event) = eventType.matches(class2eventType(event.getClass)) && eventKey.contains(event.eventDetails.eventKey)
  }
  private case class SnapshotSubscriber(commandId: CommandId, entryType: SnapshotEntryType, subscriber: ActorRef)
  private var snapshots = loadSnapshots()
  private var snapshotSubscribers = Set[SnapshotSubscriber]()
  private var subscriptions = Set[Subscription]()

  override def receive: Receive = LoggingReceive {
    case event: Event =>
      processSnapshotUpdate(event)
      processGenericSubscriptions(event)
    case Terminated(subscriber) =>
      subscriptions = subscriptions.filter(s => s.receiver != subscriber)
      snapshotSubscribers = snapshotSubscribers.filter(s => s.subscriber != subscriber)
    case command: EventBusCommand => command match {
      case Subscribe(_, subscriptionId, eventType, eventKey) =>
        sender() ! Subscribed(command.queryId, subscriptionId, eventType)
        subscriptions += Subscription(eventType, eventKey, sender(), subscriptionId)
        context.watch(sender())
      case command @ UnSubscribe(commandId, subscriptionId) =>
        sender() ! UnSubscribed(command.queryId, subscriptionId)
        subscriptions = subscriptions.filter(s => s.subscriptionId != subscriptionId)
      case command @ StartSnapshotSubscription(CommandDetails(commandId), entryType) =>
        snapshotSubscribers += SnapshotSubscriber(commandId, entryType, sender())
        sender() ! SnapshotSubscriptionStarted(EventDetails(EventId.generate(), EventKey(), Seq(commandId)), commandId,
          snapshots.getOrElse(entryType, emptySnapshot(entryType)))
        context.watch(sender())
      case command @ StopSnapshotSubscription(_, subscriptionId) =>
        snapshotSubscribers = snapshotSubscribers.filter(s => s.commandId != subscriptionId)
    }
    case command @ GetSnapshot(_, entryType) =>
      sender() ! command.success(snapshots.getOrElse(entryType, emptySnapshot(entryType)))
  }

  private def processGenericSubscriptions(event: Event) = subscriptions filter (_ matches event) foreach (_.receiver ! event)

  private def processSnapshotUpdate(event: Event) {
    event match {
      case updateEvent: SnapshotUpdate[_] =>
        val update = updateEvent.snapshotUpdate
        val entryType: SnapshotEntryType = update.entryType
        val current = snapshots.getOrElse(entryType, emptySnapshot(entryType)).asInstanceOf[Snapshot[Any]]
        val updated = current updatedWith updateEvent.snapshotUpdate.asInstanceOf[SnapshotUpdateDetails[Any]]
        snapshots += (entryType -> updated)
        snapshotSubscribers
          .filter(s => s.entryType == entryType)
          .foreach {s =>
            s.subscriber ! SnapshotUpdatedEvent(EventDetails(EventId.generate(), EventKey(), Seq(s.commandId)), s.commandId, update)
          }
        saveSnapshots(snapshots)
      case _ => // old style event, ignore
    }
  }

  def emptySnapshot(entryType: SnapshotEntryType): Snapshot[_] = {
    Snapshot(entryType, Map())
  }

  private def snapshotFile = new File(easyRiderData, "snapshot.json")

  private def saveSnapshots(snapshots: Map[SnapshotEntryType, Snapshot[_]]) = {
    FileUtils.write(snapshotFile, serializeSnapshots(snapshots))
  }

  private def loadSnapshots(): Map[SnapshotEntryType, Snapshot[_]] = {
    if (snapshotFile.exists()) {
      val string = FileUtils.readFileToString(snapshotFile)
      val snapshots = read[Seq[Snapshot[_]]](string)
      snapshots.map(snapshot => snapshot.entryType -> snapshot).toMap
    } else {
      log.info("Could not find snapshot file ({}). Starting with empty snapshots", snapshotFile)
      Map()
    }
  }

  private def serializeSnapshots(snapshots: Map[SnapshotEntryType, Snapshot[_]]): String = {
    implicit val formats = Serialization.formats(FullTypeHints(List(classOf[AnyRef]))) ++ JodaTimeSerializers.all ++ EventBusSerializers.serializers
    writePretty(snapshots.values)
  }
}

object EventBus {
  def apply(easyRiderData: File) = Props(classOf[EventBus], easyRiderData)
}
