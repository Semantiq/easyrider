package easyrider.business.core

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import easyrider.Applications._
import easyrider.Commands.{Success, Failure}
import easyrider.Events.{Subscribe, Snapshot, GetSnapshotResponse, GetSnapshot}
import easyrider.Infrastructure.{ContainerStateChangedEvent, CreateContainer}
import easyrider._
import org.scalatest.{FlatSpecLike, Matchers}

class ApplicationManagerTest extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ImplicitSender {
  "ApplicationManager" should "allow to add and remove apps" in {
    val (eventBus, _, apps) = setup()

    apps ! CreateApplication(CommandDetails(CommandId("1")), Application(applicationId, Seq()))
    expectSuccessfulCompletionOf(CommandId("1"))
    eventBus.expectMsgClass(classOf[ApplicationUpdatedEvent])

    apps ! RemoveApplication(CommandDetails(CommandId("2")), applicationId)
    expectSuccessfulCompletionOf(CommandId("2"))
    val event = eventBus.expectMsgClass(classOf[ApplicationUpdatedEvent])

    event.eventDetails shouldBe 'removal
  }

  def expectSuccessfulCompletionOf(id: CommandId): Unit = {
    expectMsgClass(classOf[Success]).executionOf should be(id)
  }

  it should "not allow to remove application with stages" in {
    val (_, _, apps) = setup()

    apps ! CreateApplication(CommandDetails(CommandId("1")), Application(applicationId, Seq()))
    expectSuccessfulCompletionOf(CommandId("1"))
    apps ! CreateStage(CommandDetails(CommandId("2")), Stage(stageId, Seq()))
    expectSuccessfulCompletionOf(CommandId("2"))
    apps ! RemoveApplication(CommandDetails(CommandId("3")), applicationId)
    expectMsgClass(classOf[Failure]).executionOf should be (CommandId("3"))
    apps ! RemoveStage(CommandDetails(CommandId("4")), stageId)
    expectSuccessfulCompletionOf(CommandId("4"))
    apps ! RemoveApplication(CommandDetails(CommandId("5")), applicationId)
    expectSuccessfulCompletionOf(CommandId("5"))
  }

  it should "deploy effective configuration to new containers" in {
    val (eventBus, infrastructure, apps) = setup()

    apps ! CreateApplication(CommandDetails(CommandId("1")), Application(applicationId, Seq(Property("literal.string", "appProperty", "appValue"))))
    expectSuccessfulCompletionOf(CommandId("1"))
    apps ! CreateStage(CommandDetails(CommandId("2")), Stage(stageId, Seq(
      Property("literal.string", "stageProperty", "stageValue"),
      Property("literal.string", "containerProperty", "defaultValue"))))
    expectSuccessfulCompletionOf(CommandId("2"))
    apps ! CreateContainerConfiguration(CommandDetails(CommandId("3")), ContainerConfiguration(containerId, NodeId("nodeA"), Seq(
      Property("literal.string", "containerProperty", "containerValue"))))
    expectSuccessfulCompletionOf(CommandId("3"))

    infrastructure.expectMsgClass(classOf[CreateContainer])
    eventBus.expectMsgClass(classOf[ApplicationUpdatedEvent])
    eventBus.expectMsgClass(classOf[StageUpdatedEvent])
    eventBus.expectMsgClass(classOf[ContainerConfigurationUpdatedEvent])
    val event = eventBus.expectMsgClass(classOf[EffectiveConfigurationChanged])
    event.effectiveConfiguration.entries.size should be (3)
  }

  private def applicationId = ApplicationId("testApp")
  private def stageId = StageId(applicationId, "qa")
  private def containerId = ContainerId(stageId, "0")
  private def setup() = {
    val eventBus = TestProbe()
    val infrastructure = TestProbe()
    val apps = system.actorOf(ApplicationManager(eventBus.ref, infrastructure.ref))
    eventBus.expectMsgClass(classOf[Subscribe]).eventType should be(EventType(ComponentId("core"), classOf[ContainerStateChangedEvent].getName))
    eventBus.expectMsgClass(classOf[GetSnapshot]).entryType should be(SnapshotEntryType(classOf[Application]))
    eventBus.reply(GetSnapshotResponse(EventDetails(EventId.generate(), EventKey(), Seq()), Snapshot(SnapshotEntryType(classOf[Application]), Map()), CommandId("?")))
    eventBus.expectMsgClass(classOf[GetSnapshot]).entryType should be(SnapshotEntryType(classOf[Stage]))
    eventBus.reply(GetSnapshotResponse(EventDetails(EventId.generate(), EventKey(), Seq()), Snapshot(SnapshotEntryType(classOf[Stage]), Map()), CommandId("?")))
    eventBus.expectMsgClass(classOf[GetSnapshot]).entryType should be(SnapshotEntryType(classOf[ContainerConfiguration]))
    eventBus.reply(GetSnapshotResponse(EventDetails(EventId.generate(), EventKey(), Seq()), Snapshot(SnapshotEntryType(classOf[ContainerConfiguration]), Map()), CommandId("?")))
    (eventBus, infrastructure, apps)
  }
}
