package easyrider.business.core

import java.util.concurrent.TimeUnit

import akka.actor.{Props, Actor, ActorLogging, ActorRef}
import akka.pattern._
import akka.util.Timeout
import easyrider.Applications.StageUpdatedEvent
import easyrider.Events.{GetSnapshot, GetSnapshotResponse, Subscribe, Subscribed}
import easyrider.Implicits._
import easyrider.Repository.VersionAvailableEvent
import easyrider.{CommandDetails, EventKey, QueryId}

class ReleaseManager(eventBus: ActorRef, orchestrator: ActorRef) extends Actor with ActorLogging {
  implicit val dispatcher = context.system.dispatcher
  implicit val timeout = Timeout(1, TimeUnit.SECONDS)
  eventBus ! Subscribe(CommandDetails(), "new-versions", classOf[VersionAvailableEvent], EventKey())

  override def receive: Receive = {
    case _: Subscribed[_] => // TODO: check for any missed events
    case VersionAvailableEvent(eventDetails, version) =>
      eventBus ? GetSnapshot(QueryId.generate(), classOf[StageUpdatedEvent]) onSuccess {
        case configuration: GetSnapshotResponse[StageUpdatedEvent] =>
        val stages = configuration.snapshot.map(_.stage).filter(_.id.applicationId == version.applicationId)
        log.info("Considering deployment of {} to {}", version, stages)
      }
  }
}

object ReleaseManager {
  def apply(eventBus: ActorRef, orchestrator: ActorRef) = Props(classOf[ReleaseManager], eventBus, orchestrator)
}
