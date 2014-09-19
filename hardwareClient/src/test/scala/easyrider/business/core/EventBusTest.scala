package easyrider.business.core

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import easyrider.Applications.{ApplicationId, Application, ApplicationUpdatedEvent}
import easyrider.Events._
import easyrider.Infrastructure.{NodeId, NodeCreated, NodeUpdatedEvent}
import easyrider._
import easyrider.business.core
import org.apache.commons.io.FileUtils
import org.joda.time.DateTime
import org.scalatest._
import easyrider.Implicits._

class EventBusTest() extends TestKit(ActorSystem()) with FlatSpecLike with Matchers {
  "EventBus" should "send events to subscribers" in {
    val bus = system.actorOf(core.EventBus(emptyDirectory))

    val publisher = TestProbe()
    val subscriber = TestProbe()

    subscriber.send(bus, dummySubscribe)
    subscriber.expectMsgClass(classOf[Subscribed[_]])
    publisher.send(bus, dummyEvent)
    subscriber.expectMsgClass(classOf[NodeUpdatedEvent])
  }

  it should "not send events after un-subscribe" in {
    val bus = system.actorOf(core.EventBus(emptyDirectory))

    val publisher = TestProbe()
    val subscriber = TestProbe()

    subscriber.send(bus, dummySubscribe)
    subscriber.expectMsgClass(classOf[Subscribed[_]])
    publisher.send(bus, dummyEvent)
    subscriber.expectMsgClass(classOf[NodeUpdatedEvent])
    subscriber.send(bus, UnSubscribe(CommandId("1"), dummySubscribe.subscriptionId))
    subscriber.expectMsgClass(classOf[UnSubscribed])
    publisher.send(bus, dummyEvent)
    subscriber.expectNoMsg()
  }

  it should "handle removal events" in {
    val bus = system.actorOf(core.EventBus(emptyDirectory))
    val client = TestProbe()
    client.send(bus, NodeUpdatedEvent(EventDetails(EventId("1"), EventKey("node0"), Seq(CommandId("1"))), NodeId("node0"), NodeCreated))
    client.send(bus, NodeUpdatedEvent(EventDetails(EventId("2"), EventKey("node0"), Seq(CommandId("2")), removal = true), NodeId("node0"), NodeCreated))
    client.send(bus, Subscribe(CommandId.generate(), "node-events", classOf[NodeUpdatedEvent], EventKey()))
    val subscribed = client.expectMsgClass(classOf[Subscribed[_]])
    subscribed.snapshot should be ('empty)
  }

  it should "replay events matching a subscription" in {
    val bus = system.actorOf(core.EventBus(emptyDirectory))
    val client = TestProbe()
    client.send(bus, NodeUpdatedEvent(EventDetails(EventId("1"), EventKey("node0"), Seq(CommandId("1"))), NodeId("node0"), NodeCreated))
    client.send(bus, NodeUpdatedEvent(EventDetails(EventId("2"), EventKey("node0"), Seq(CommandId("2")), removal = true), NodeId("node0"), NodeCreated))
    client.send(bus, ApplicationUpdatedEvent(EventDetails(EventId("3"), EventKey("app"), Seq(CommandId("3"))), Application(ApplicationId("app"), Seq())))
    client.send(bus, Subscribe(CommandId.generate(), "node-events", classOf[NodeUpdatedEvent], EventKey()))
    client.expectMsgClass(classOf[Subscribed[_]])
    client.send(bus, GetReplay(QueryId.generate(), Seq("node-events"), DateTime.now().minusMinutes(1)))
    val replay = client.expectMsgClass(classOf[GetReplayResponse])
    replay.events.size should be (2)
  }

  private def emptyDirectory: File = {
    val dir = new File("target/easyrider")
    FileUtils.deleteDirectory(dir)
    dir
  }

  val dummySubscribe = Subscribe(CommandId("1"), "all", classOf[NodeUpdatedEvent], EventKey())
  val dummyEvent = NodeUpdatedEvent(EventDetails(EventId("1"), EventKey(), Seq()), NodeId("nodeId"), NodeCreated)
}
