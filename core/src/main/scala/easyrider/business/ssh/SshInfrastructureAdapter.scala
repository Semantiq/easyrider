package easyrider.business.ssh

import akka.actor.ActorRef
import akka.util.Timeout
import easyrider.Applications.ContainerConfigurationUpdatedEvent
import easyrider.Events.{GetSnapshotResponse, GetSnapshot}
import easyrider.business.ssh.SshInfrastructure.CommandAndSubscribe
import easyrider.{QueryId, Infrastructure}
import easyrider.Infrastructure._
import org.reactivestreams.Subscriber

import scala.concurrent.{Promise, Future}

import easyrider.Implicits._
import akka.pattern.Patterns.ask

class SshInfrastructureAdapter(sshInfrastructure: ActorRef, eventBus: ActorRef, applicationManager: ActorRef, timeout: Timeout) extends Infrastructure.InfrastructureInterface {
  override def nodes(changeListener: Subscriber[NodeUpdatedEvent]): Future[Seq[NodeUpdatedEvent]] = {
    val promise = Promise[Seq[NodeUpdatedEvent]]()
    sshInfrastructure ! SshInfrastructureProvider.SubscribeNodeUpdatedEvent(changeListener, promise)
    promise.future
  }

  // def unDeployVersion
  override def startContainer(command: StartContainer)(progressListener: Subscriber[ContainerStateChangedEvent]): Unit = ???

  // def destroyContainer
  override def deployVersion(command: DeployVersion)(progressListener: Subscriber[VersionDeploymentProgressEvent]) {
    applicationManager ! CommandAndSubscribe(command, progressListener)
  }

  override def createContainer(command: CreateContainer)(progressListener: Subscriber[ContainerStateChangedEvent]): Unit = ???

  override def stopContainer(command: StopContainer)(progressListener: Subscriber[ContainerStateChangedEvent]): Unit = ???

  override def containerConfigurations: Future[Seq[ContainerConfigurationUpdatedEvent]] = {
    ask(eventBus, GetSnapshot(QueryId.generate(), classOf[ContainerConfigurationUpdatedEvent]), timeout)
      .mapTo[GetSnapshotResponse[ContainerConfigurationUpdatedEvent]].map(r => r.snapshot)
  }

  override def containerStates: Future[Seq[ContainerStateChangedEvent]] = {
    ask(eventBus, GetSnapshot(QueryId.generate(), classOf[ContainerStateChangedEvent]), timeout)
      .mapTo[GetSnapshotResponse[ContainerStateChangedEvent]].map(r => r.snapshot)
  }
}
