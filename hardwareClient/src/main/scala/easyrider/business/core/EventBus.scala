package easyrider.business.core

import java.io.File

import akka.actor.{Actor, ActorRef, Props, Terminated}
import akka.event.LoggingReceive
import easyrider.Implicits._
import easyrider.{Event, EventKey, EventType}
import org.apache.commons.io.FileUtils
import org.json4s.FullTypeHints
import org.json4s.ext.JodaTimeSerializers
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, writePretty}

class EventBus(easyriderData: File) extends Actor {
  import easyrider.Events._
  private implicit val formats = Serialization.formats(FullTypeHints(List(classOf[AnyRef]))) ++ JodaTimeSerializers.all

  private case class Subscription(eventType: EventType, eventKey: EventKey, receiver: ActorRef, subscriptionId: String) {
    def matches(event: Event) = eventType == class2eventType(event.getClass) && eventKey.contains(event.eventDetails.eventKey)
  }
  private var snapshots = load()
  private var subscriptions = Set[Subscription]()
  private var eventLog = Seq[Event]()

  override def receive: Receive = LoggingReceive {
    case event: Event =>
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
      eventLog +:= event
    case Terminated(subscriber) =>
      subscriptions = subscriptions.filter(s => s.receiver != subscriber)
    case command: EventBusCommand => command match {
      case Subscribe(commandId, subscriptionId, eventType, eventKey) =>
        val snapshot = snapshots.getOrElse(eventType, Map()).values
          .filter(event => eventKey.contains(event.eventDetails.eventKey))
          .toSeq
        sender() ! Subscribed(command.queryId, subscriptionId, eventType, snapshot)
        subscriptions += Subscription(eventType, eventKey, sender(), subscriptionId)
        context.watch(sender())
      case command @ UnSubscribe(commandId, subscriptionId) =>
        sender() ! UnSubscribed(command.queryId, subscriptionId)
        subscriptions = subscriptions.filter(s => s.subscriptionId != subscriptionId)
    }
    case GetSnapshot(queryId, eventType) =>
      sender() ! GetSnapshotResponse(queryId, snapshots.getOrElse(eventType, Map()).values.toSeq)
    case GetReplay(queryId, subscriptionIds, since) =>
      val filter = subscriptions.filter(s => subscriptionIds.contains(s.subscriptionId))
      val withinTime = eventLog.dropWhile(e => e.eventDetails.publicationTime isBefore since)
      val matching = withinTime.filter(e => filter.exists(f => f.matches(e)))
      sender() ! GetReplayResponse(queryId, matching)
  }

  private def snapshotFile = new File(easyriderData, "snapshot.json")

  private def save(snapshots: Map[EventType, Map[EventKey, Event]]) = {
    val events = snapshots.values.flatMap(_.values)
    FileUtils.write(snapshotFile, writePretty(events))
  }

  private def load(): Map[EventType, Map[EventKey, Event]] = {
    if (snapshotFile.exists()) {
      val string = FileUtils.readFileToString(snapshotFile)
      val events = read[Seq[Event]](string)
      events.groupBy(event => class2eventType(event.getClass)).map {
        case (key, value) => (key, value.groupBy(event => event.eventDetails.eventKey).map {
          case (eventKey, Seq(event)) => (eventKey, event)
        })
      }
    } else {
      Map()
    }
  }
}

object EventBus {
  def apply(easyriderData: File) = Props(classOf[EventBus], easyriderData)
}
