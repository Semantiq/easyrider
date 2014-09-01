package easyrider.business.core

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestProbe, ImplicitSender, TestKit}
import easyrider.Applications._
import easyrider.Events.{GetSnapshotResponse, GetSnapshot}
import easyrider.Implicits.class2eventType
import easyrider.{QueryId, Failure, CommandId}
import org.scalatest.{Matchers, FlatSpecLike}

class ApplicationManagerTest extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ImplicitSender {
  private def applicationId = ApplicationId("testApp")
  private def stageId = StageId(applicationId, "qa")
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
    (eventBus, apps)
  }

  "ApplicationManager" should "allow to add and remove apps" in {
    val (eventBus, apps) = setup()

    apps ! CreateApplication(CommandId.generate(), Application(applicationId, Seq()))

    eventBus.expectMsgClass(classOf[ApplicationUpdatedEvent])

    apps ! RemoveApplication(CommandId.generate(), applicationId)
    val event = eventBus.expectMsgClass(classOf[ApplicationUpdatedEvent])

    event.eventDetails.removal should be (true)
  }

  it should "not allow to remove application with stages" in {
    val (_, apps) = setup()

    apps ! CreateApplication(CommandId("1"), Application(applicationId, Seq()))
    apps ! CreateStage(CommandId("2"), Stage(stageId, Seq()))
    apps ! RemoveApplication(CommandId("3"), applicationId)
    expectMsgClass(classOf[Failure]).commandId should be (CommandId("3"))
    apps ! RemoveStage(CommandId("4"), stageId)
    apps ! RemoveApplication(CommandId("5"), applicationId)
    expectNoMsg()
  }
}
