package easyrider.business.core

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import easyrider.Api.{Authentication, AuthenticateUser}
import easyrider.Applications._
import easyrider.Infrastructure.{NodeId, NodeCreated, NodeUpdatedEvent}
import easyrider._
import org.scalatest.{FlatSpecLike, Matchers}

// TODO: Better name required
class ApiActorTest() extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ImplicitSender {
  "ApiActor" should "die after using without authentication" in {
    val (_, _, client, api) = setup()
    val watcher = TestProbe()
    watcher watch api

    client.send(api, NodeUpdatedEvent(EventDetails(EventId("1"), EventKey(), Seq()), NodeId("node0"), NodeCreated))

    watcher.expectTerminated(api)
  }

  it should "transfer events in two ways after authentication" in {
    def dummyEvent = NodeUpdatedEvent(EventDetails(EventId(UUID.randomUUID.toString), EventKey(), Seq()), NodeId("node0"),
      NodeCreated)
    val (_, bus, client, api) = setup()

    client.send(api, AuthenticateUser())

    client.expectMsg(Authentication())

    val event1 = dummyEvent
    client.send(api, event1)
    bus.expectMsg(event1)

    val event2 = dummyEvent
    bus.send(api, event2)
    client.expectMsg(event2)
  }
  it should "add application and indicate it by event" in {
    val (applicationManager, bus, client, api) = setup()
    val command: ApplicationCommand = CreateApplication(CommandDetails(CommandId.generate(), TraceMode()), Application(ApplicationId("app"), Seq()))
    client.send(api, AuthenticateUser())

    client.send(api, command)
    applicationManager.expectMsg(command)
  }
  def setup(): (TestProbe, TestProbe, TestProbe, ActorRef) = {
    val applicationManager = TestProbe()
    val bus = TestProbe()
    val componentManager = TestProbe()
    val infrastructure = TestProbe()
    val repositoryStorage = TestProbe()
    val client = TestProbe()
    val orchestrator = TestProbe()
    val api = system.actorOf(ApiActor(bus.ref, applicationManager.ref, componentManager.ref, infrastructure.ref, repositoryStorage.ref, orchestrator.ref)(client.ref))
    (applicationManager, bus, client, api)
  }

}
