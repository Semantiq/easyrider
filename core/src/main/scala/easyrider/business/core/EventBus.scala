package easyrider.business.core

import java.io.File

import akka.actor._
import akka.event.LoggingReceive
import easyrider.Implicits._
import easyrider.{Event, EventKey, EventType}
import org.apache.commons.io.FileUtils
import org.json4s.JsonAST.JArray
import org.json4s.ext.JodaTimeSerializers
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, writePretty}
import org.json4s.{FullTypeHints, JValue}

class EventBus(easyRiderData: File) extends Actor with ActorLogging {
  import easyrider.Events._
  private implicit val formats = Serialization.formats(FullTypeHints(List(classOf[AnyRef]))) ++ JodaTimeSerializers.all

  private case class Subscription(eventType: EventType, eventKey: EventKey, receiver: ActorRef, subscriptionId: String) {
    def matches(event: Event) = eventType.matches(class2eventType(event.getClass)) && eventKey.contains(event.eventDetails.eventKey)
  }
  private var snapshots = loadSnapshot()
  private var subscriptions = Set[Subscription]()
  private var eventLog = loadEventLog()

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
      save(eventLog)
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
      case command @ SubscribeToCommandTrail(commandDetails, _, trace) =>
        val tracer = context.actorOf(CommandTracer(sender(), command))
        for (eventType <- trace) {
          subscriptions += Subscription(eventType, EventKey(), tracer, commandDetails.commandId.id + "_" + eventType.getSimpleName)
        }
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

  private def snapshotFile = new File(easyRiderData, "snapshot.json")
  private def eventLogFile = new File(easyRiderData, "eventLogFile.json")

  private def save(snapshots: Map[EventType, Map[EventKey, Event]]) = {
    val events = snapshots.values.flatMap(_.values)
    FileUtils.write(snapshotFile, writePretty(events))
  }

  private def save(eventLog: Seq[Event]) {
    FileUtils.write(eventLogFile, writePretty(eventLog))
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
}
