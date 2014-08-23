package easyrider.business.core

import akka.actor.ActorSystem
import akka.testkit.{TestProbe, ImplicitSender, TestKit}
import easyrider.Applications._
import easyrider.CommandId
import org.scalatest.{Matchers, FlatSpecLike}

class ApplicationManagerTest extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ImplicitSender {
  "ApplicationManager" should "allow to add and remove apps" in {
    val eventBus = TestProbe()
    val apps = system.actorOf(ApplicationManager(eventBus.ref))

    apps ! CreateApplication(CommandId.generate(), Application(ApplicationId("test-app"), Seq()))

    eventBus.expectMsgClass(classOf[ApplicationUpdatedEvent])

    apps ! RemoveApplication(CommandId.generate(), ApplicationId("test-app"))

    val event = eventBus.expectMsgClass(classOf[ApplicationUpdatedEvent])
    event.eventDetails.removal should be (true)
  }
}
