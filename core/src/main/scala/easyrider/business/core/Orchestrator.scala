package easyrider.business.core

import akka.actor.{Actor, ActorLogging, Props}
import akka.event.LoggingReceive
import easyrider.Orchestrator.ReleaseVersionToStage

class Orchestrator(releaseFactory: ReleaseVersionToStage => Props) extends Actor with ActorLogging {
  override def receive = LoggingReceive {
    case release: ReleaseVersionToStage =>
      // TODO: reject releases when already in progress
      context.actorOf(releaseFactory(release), release.stageId.applicationId.id + "-" + release.stageId.id)
  }
}

object Orchestrator {
  def apply(releaseFactory: ReleaseVersionToStage => Props) = Props(classOf[Orchestrator], releaseFactory)
}
