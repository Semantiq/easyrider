package easyrider.business.core

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import easyrider.Applications._
import easyrider.Events.{Subscribed, Subscribe}
import easyrider.Implicits.class2eventType
import easyrider.Infrastructure._
import easyrider.Orchestrator.{ReleaseSuccessful, ReleaseEvent, ReleaseVersionToStage}
import easyrider.Repository.Version
import easyrider._
import org.scalatest.{FlatSpecLike, Matchers}

class OrchestratorTest extends TestKit(ActorSystem()) with FlatSpecLike with Matchers {
  "Orchestrator" should "perform rolling release" in {
    val (orchestrator, eventBus, commandCenter) = setup()

    orchestrator ! ReleaseVersionToStage(CommandDetails(), StageId(ApplicationId("app"), "dev"), Version(ApplicationId("app"), "1.0.0"))

    val subscribe = eventBus.expectMsgClass(classOf[Subscribe])
    subscribe.eventType should be(class2eventType(classOf[ContainerConfigurationUpdatedEvent]))
    subscribe.eventKey should be(EventKey("app", "dev"))

    eventBus.send(eventBus.lastSender, Subscribed(subscribe.queryId, subscribe.subscriptionId, classOf[ContainerConfigurationUpdatedEvent], Seq(
      ContainerConfigurationUpdatedEvent(EventDetails(EventId("1"), EventKey("app", "dev"), Seq()), ContainerConfiguration(ContainerId(StageId(ApplicationId("app"), "dev"), "0"), NodeId("dev0"), Seq())),
      ContainerConfigurationUpdatedEvent(EventDetails(EventId("2"), EventKey("app", "dev"), Seq()), ContainerConfiguration(ContainerId(StageId(ApplicationId("app"), "dev"), "1"), NodeId("dev1"), Seq()))
    )))

    val subscribeDeployment = eventBus.expectMsgClass(classOf[Subscribe])
    subscribeDeployment.eventType should be (class2eventType(classOf[VersionDeploymentProgressEvent]))
    subscribeDeployment.eventKey should be (EventKey("app", "dev"))
    val deploymentEvents = eventBus.lastSender
    eventBus.send(deploymentEvents, Subscribed(subscribeDeployment.queryId, subscribeDeployment.subscriptionId, classOf[VersionDeploymentProgressEvent], Seq()))

    val subscribeContainerState = eventBus.expectMsgClass(classOf[Subscribe])
    subscribeContainerState.eventType should be (class2eventType(classOf[ContainerStateChangedEvent]))
    val containerStateEvents = eventBus.lastSender
    eventBus.send(containerStateEvents, Subscribed(subscribeContainerState.queryId, subscribeContainerState.subscriptionId, classOf[ContainerStateChangedEvent], Seq()))

    val deployToNode0 = commandCenter.expectMsgClass(classOf[DeployVersion])
    val deployToNode1 = commandCenter.expectMsgClass(classOf[DeployVersion])

    deploymentEvents ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), EventKey("app", "dev", "0", "1.0.0"), Seq(deployToNode0.commandDetails.commandId)), Version(ApplicationId("app"), "1.0.0"), DeploymentCompleted)
    deploymentEvents ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), EventKey("app", "dev", "1", "1.0.0"), Seq(deployToNode1.commandDetails.commandId)), Version(ApplicationId("app"), "1.0.0"), DeploymentCompleted)

    val startContainer0 = commandCenter.expectMsgClass(classOf[StartContainer])
    commandCenter.expectNoMsg()
    containerStateEvents ! ContainerStateChangedEvent(EventDetails(EventId.generate(), EventKey("app", "dev", "0"), Seq(startContainer0.commandDetails.commandId)), ContainerId(StageId(ApplicationId("app"), "dev"), "0"), ContainerRunning(Version(ApplicationId("app"), "1.0.0")))
    val startContainer1 = commandCenter.expectMsgClass(classOf[StartContainer])
    containerStateEvents ! ContainerStateChangedEvent(EventDetails(EventId.generate(), EventKey("app", "dev", "1"), Seq(startContainer1.commandDetails.commandId)), ContainerId(StageId(ApplicationId("app"), "dev"), "0"), ContainerRunning(Version(ApplicationId("app"), "1.0.0")))

    eventBus.expectMsgClass(classOf[ReleaseEvent]).releaseStatus should be (ReleaseSuccessful(Version(ApplicationId("app"), "1.0.0")))
  }

  private def setup() = {
    val eventBus = TestProbe()
    val commandCenter = TestProbe()
    val releaseFactory = OrchestratedDeployment(eventBus.ref, commandCenter.ref) _
    val orchestrator = system.actorOf(Orchestrator(releaseFactory))
    (orchestrator, eventBus, commandCenter)
  }
}
