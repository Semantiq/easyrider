package easyrider.business.core

import akka.actor.ActorSystem
import akka.testkit.{TestProbe, ImplicitSender, TestKit}
import easyrider.Applications._
import easyrider.{Failure, CommandId}
import org.scalatest.{Matchers, FlatSpecLike}

class ApplicationManagerTest extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ImplicitSender {
  "ApplicationManager" should "allow to add and remove apps" in {
    val eventBus = TestProbe()
    val apps = system.actorOf(ApplicationManager(eventBus.ref))

    apps ! CreateApplication(CommandId.generate(), Application(applicationId, Seq()))

    eventBus.expectMsgClass(classOf[ApplicationUpdatedEvent])

    apps ! RemoveApplication(CommandId.generate(), applicationId)
    val event = eventBus.expectMsgClass(classOf[ApplicationUpdatedEvent])

    event.eventDetails.removal should be (true)
  }

  it should "not allow to remove application with stages" in {
    val eventBus = TestProbe()
    val apps = system.actorOf(ApplicationManager(eventBus.ref))

    apps ! CreateApplication(CommandId("1"), Application(applicationId, Seq()))
    apps ! CreateStage(CommandId("2"), Stage(stageId, Seq()))
    apps ! RemoveApplication(CommandId("3"), applicationId)
    expectMsgClass(classOf[Failure]).commandId should be (CommandId("3"))
    apps ! RemoveStage(CommandId("4"), stageId)
    apps ! RemoveApplication(CommandId("5"), applicationId)
    expectNoMsg()
  }
  val applicationId = ApplicationId("testApp")
  val stageId = StageId(applicationId, "qa")
}
