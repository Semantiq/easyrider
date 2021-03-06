package easyrider.business.core

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import easyrider.Api.{AuthenticateUser, Authentication}
import easyrider.Applications._
import easyrider.Infrastructure.{NodeCreated, NodeState, NodeUpdatedEvent}
import easyrider._
import org.scalatest.{FlatSpecLike, Matchers}

// TODO: Better name required
class ApiActorTest() extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ImplicitSender {
  "ApiActor" should "die after using without authentication" in {
    val (_, _, client, api) = setup()
    val watcher = TestProbe()
    watcher watch api

    client.send(api, NodeUpdatedEvent(EventDetails(EventId("1")), SnapshotUpdateDetails(SnapshotEntryType(classOf[NodeState]), NodeId("node0").eventKey, Some(NodeCreated))))

    watcher.expectTerminated(api)
  }

  it should "transfer events in two ways after authentication" in {
    def dummyEvent = NodeUpdatedEvent(EventDetails(EventId.generate()), SnapshotUpdateDetails(SnapshotEntryType(classOf[NodeState]), NodeId("node0").eventKey, Some(NodeCreated)))
    val (_, bus, client, api) = setup()

    client.send(api, AuthenticateUser(CommandDetails(CommandId("1")), "test", "test"))

    client.expectMsgClass(classOf[Authentication]).username shouldBe "test"

    val event1 = dummyEvent
    client.send(api, event1)
    bus.expectMsg(event1)

    val event2 = dummyEvent
    bus.send(api, event2)
    client.expectMsg(event2)
  }
  it should "add application and indicate it by event" in {
    val (applicationManager, bus, client, api) = setup()
    val command: ApplicationCommand = CreateApplication(CommandDetails(), Application(ApplicationId("app"), Seq()))
    client.send(api, AuthenticateUser(CommandDetails(CommandId("1")), "test", "test"))

    client.send(api, command)
    applicationManager.expectMsg(command)
  }
  def setup(): (TestProbe, TestProbe, TestProbe, ActorRef) = {
    val applicationManager = TestProbe()
    val bus = TestProbe()
    val componentManager = TestProbe()
    val infrastructure = TestProbe()
    val client = TestProbe()
    val orchestrator = TestProbe()
    val authenticator = TestProbe()
    val nodeManager = TestProbe()
    val api = system.actorOf(ApiActor(bus.ref, applicationManager.ref, componentManager.ref, infrastructure.ref, orchestrator.ref, authenticator.ref, nodeManager.ref)(client.ref))
    (applicationManager, bus, client, api)
  }

}
