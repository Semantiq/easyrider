package easyrider.business.core

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestProbe, ImplicitSender, TestKit}
import easyrider.Applications._
import easyrider.Events.{GetSnapshotResponse, GetSnapshot}
import easyrider.Implicits.class2eventType
import easyrider._
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

    apps ! CreateApplication(CommandDetails(CommandId.generate(), TraceMode()), Application(applicationId, Seq()))

    eventBus.expectMsgClass(classOf[ApplicationUpdatedEvent])

    apps ! RemoveApplication(CommandDetails(CommandId.generate(), TraceMode()), applicationId)
    val event = eventBus.expectMsgClass(classOf[ApplicationUpdatedEvent])

    event.eventDetails.removal should be (true)
  }

  it should "not allow to remove application with stages" in {
    val (_, apps) = setup()

    apps ! CreateApplication(CommandDetails(CommandId("1"), TraceMode()), Application(applicationId, Seq()))
    apps ! CreateStage(CommandDetails(CommandId("2"), TraceMode()), Stage(stageId, Seq()))
    apps ! RemoveApplication(CommandDetails(CommandId("3"), TraceMode()), applicationId)
    expectMsgClass(classOf[Failure]).commandId should be (CommandId("3"))
    apps ! RemoveStage(CommandDetails(CommandId.generate(), TraceMode()), stageId)
    apps ! RemoveApplication(CommandDetails(CommandId.generate(), TraceMode()), applicationId)
    expectNoMsg()
  }
}
