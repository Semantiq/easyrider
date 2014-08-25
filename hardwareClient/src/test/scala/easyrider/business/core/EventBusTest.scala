package easyrider.business.core

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import easyrider.Events.{Subscribe, Subscribed, UnSubscribe, UnSubscribed}
import easyrider.Infrastructure.{NodeCreated, NodeUpdatedEvent}
import easyrider._
import easyrider.business.core
import org.scalatest._
import easyrider.Implicits._

class EventBusTest() extends TestKit(ActorSystem()) with FlatSpecLike with Matchers {
  "EventBus" should "send events to subscribers" in {
    val bus = system.actorOf(core.EventBus())

    val publisher = TestProbe()
    val subscriber = TestProbe()

    subscriber.send(bus, dummySubscribe)
    subscriber.expectMsgClass(classOf[Subscribed[_]])
    publisher.send(bus, dummyEvent)
    subscriber.expectMsgClass(classOf[NodeUpdatedEvent])
  }

  it should "not send events after un-subscribe" in {
    val bus = system.actorOf(core.EventBus())

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
    val bus = system.actorOf(core.EventBus())
    val client = TestProbe()
    client.send(bus, NodeUpdatedEvent(EventDetails(EventId("1"), EventKey("node0"), Seq(CommandId("1"))), NodeCreated))
    client.send(bus, NodeUpdatedEvent(EventDetails(EventId("2"), EventKey("node0"), Seq(CommandId("2")), removal = true), NodeCreated))
    client.send(bus, Subscribe(CommandId.generate(), "node-events", classOf[NodeUpdatedEvent], EventKey()))
    val subscribed = client.expectMsgClass(classOf[Subscribed[_]])
    subscribed.snapshot should be ('empty)
  }

  val dummySubscribe = Subscribe(CommandId("1"), "all", classOf[NodeUpdatedEvent], EventKey())
  val dummyEvent = NodeUpdatedEvent(EventDetails(EventId("1"), EventKey(), Seq()), NodeCreated)
}
