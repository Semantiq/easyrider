package easyrider.business.core

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import easyrider.Applications._
import easyrider.Commands.Failure
import easyrider.Configuration.{EffectiveConfiguration, DeployConfiguration}
import easyrider.Events.{GetSnapshot, GetSnapshotResponse}
import easyrider.Implicits.class2eventType
import easyrider.Infrastructure.{CreateContainer, AddressedContainerCommand, NodeId}
import easyrider._
import org.scalatest.{FlatSpecLike, Matchers}

class ApplicationManagerTest extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ImplicitSender {
  "ApplicationManager" should "allow to add and remove apps" in {
    val (eventBus, _, apps) = setup()

    apps ! CreateApplication(CommandDetails(), Application(applicationId, Seq()))

    eventBus.expectMsgClass(classOf[ApplicationUpdatedEvent])

    apps ! RemoveApplication(CommandDetails(), applicationId)
    val event = eventBus.expectMsgClass(classOf[ApplicationUpdatedEvent])

    event.eventDetails shouldBe 'removal
  }

  it should "not allow to remove application with stages" in {
    val (_, _, apps) = setup()

    apps ! CreateApplication(CommandDetails(CommandId("1")), Application(applicationId, Seq()))
    apps ! CreateStage(CommandDetails(CommandId("2")), Stage(stageId, Seq()))
    apps ! RemoveApplication(CommandDetails(CommandId("3")), applicationId)
    expectMsgClass(classOf[Failure]).eventDetails.causedBy should contain (CommandId("3"))
    apps ! RemoveStage(CommandDetails(), stageId)
    apps ! RemoveApplication(CommandDetails(), applicationId)
    expectNoMsg()
  }

  it should "deploy effective configuration to new containers" in {
    val (_, infrastructure, apps) = setup()

    apps ! CreateApplication(CommandDetails(CommandId("1")), Application(applicationId, Seq(Property("literal.string", "appProperty", "appValue"))))
    apps ! CreateStage(CommandDetails(CommandId("2")), Stage(stageId, Seq(
      Property("literal.string", "stageProperty", "stageValue"),
      Property("literal.string", "containerProperty", "defaultValue"))))
    apps ! CreateContainerConfiguration(CommandDetails(CommandId("3")), ContainerConfiguration(containerId, NodeId("nodeA"), Seq(
      Property("literal.string", "containerProperty", "containerValue"))))

    infrastructure.expectMsgClass(classOf[CreateContainer])
    val command = infrastructure.expectMsgClass(classOf[AddressedContainerCommand]).containerCommand.asInstanceOf[DeployConfiguration]
    command.configuration should be (EffectiveConfiguration(Map("appProperty" -> "appValue", "stageProperty" -> "stageValue", "containerProperty" -> "containerValue")))
  }

  private def applicationId = ApplicationId("testApp")
  private def stageId = StageId(applicationId, "qa")
  private def containerId = ContainerId(stageId, "0")
  private def setup() = {
    val eventBus = TestProbe()
    val infrastructure = TestProbe()
    val apps = system.actorOf(ApplicationManager(eventBus.ref, infrastructure.ref))
    eventBus.expectMsgClass(classOf[GetSnapshot]).eventType should be(class2eventType(classOf[ApplicationUpdatedEvent]))
    eventBus.reply(GetSnapshotResponse(QueryId("any"), Seq()))
    eventBus.expectMsgClass(classOf[GetSnapshot]).eventType should be(class2eventType(classOf[StageUpdatedEvent]))
    eventBus.reply(GetSnapshotResponse(QueryId("any"), Seq()))
    eventBus.expectMsgClass(classOf[GetSnapshot]).eventType should be(class2eventType(classOf[ContainerConfigurationUpdatedEvent]))
    eventBus.reply(GetSnapshotResponse(QueryId("any"), Seq()))
    (eventBus, infrastructure, apps)
  }
}
