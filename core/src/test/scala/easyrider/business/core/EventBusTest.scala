package easyrider.business.core

import java.io.File

import akka.actor.{ActorSystem, PoisonPill, Terminated}
import akka.testkit.{TestKit, TestProbe}
import easyrider.Api.CommandSentEvent
import easyrider.Applications.{Application, ApplicationId, ApplicationUpdatedEvent}
import easyrider.Commands.{Success, CommandExecution}
import easyrider.Events._
import easyrider.Implicits._
import easyrider.Infrastructure.{NodeState, NodeCreated, NodeId, NodeUpdatedEvent}
import easyrider._
import easyrider.business.core
import org.apache.commons.io.FileUtils
import org.joda.time.DateTime
import org.scalatest._

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
    subscriber.send(bus, UnSubscribe(CommandDetails(), dummySubscribe.subscriptionId))
    subscriber.expectMsgClass(classOf[UnSubscribed])
    publisher.send(bus, dummyEvent)
    subscriber.expectNoMsg()
  }

  it should "handle removal events" in {
    val bus = system.actorOf(core.EventBus(emptyDirectory))
    val client = TestProbe()
    client.send(bus, NodeUpdatedEvent(EventDetails(EventId("1"), EventKey("node0"), Seq(CommandId("1"))), NodeId("node0"), NodeCreated))
    client.send(bus, NodeUpdatedEvent(EventDetails(EventId("2"), EventKey("node0"), Seq(CommandId("2")), removal = true), NodeId("node0"), NodeCreated))
    client.send(bus, Subscribe(CommandDetails(), "node-events", classOf[NodeUpdatedEvent], EventKey()))
    val subscribed = client.expectMsgClass(classOf[Subscribed[_]])
    subscribed.snapshot should be ('empty)
  }

  it should "replay events matching a subscription" in {
    val bus = system.actorOf(core.EventBus(emptyDirectory))
    val client = TestProbe()
    client.send(bus, NodeUpdatedEvent(EventDetails(EventId("1"), EventKey("node0"), Seq(CommandId("1"))), NodeId("node0"), NodeCreated))
    client.send(bus, NodeUpdatedEvent(EventDetails(EventId("2"), EventKey("node0"), Seq(CommandId("2")), removal = true), NodeId("node0"), NodeCreated))
    client.send(bus, ApplicationUpdatedEvent(EventDetails(EventId("3"), EventKey("app"), Seq(CommandId("3"))), Application(ApplicationId("app"), Seq())))
    client.send(bus, Subscribe(CommandDetails(), "node-events", classOf[NodeUpdatedEvent], EventKey()))
    client.expectMsgClass(classOf[Subscribed[_]])
    client.send(bus, GetReplay(QueryId.generate(), Seq("node-events"), DateTime.now().minusMinutes(1)))
    val replay = client.expectMsgClass(classOf[GetReplayResponse])
    replay.events.size should be (2)
  }

  it should "recover events from file" in {
    val directory = emptyDirectory
    val bus0 = system.actorOf(core.EventBus(directory))
    val client = TestProbe()
    client.send(bus0, ApplicationUpdatedEvent(EventDetails(EventId("3"), EventKey("app"), Seq(CommandId("3"))), Application(ApplicationId("app"), Seq())))
    client.watch(bus0)
    client.send(bus0, PoisonPill)
    client.expectMsgClass(classOf[Terminated])
    val bus1 = system.actorOf(core.EventBus(directory))
    client.send(bus1, Subscribe(CommandDetails(), "app-events", classOf[ApplicationUpdatedEvent], EventKey()))
    client.send(bus1, GetReplay(QueryId.generate(), Seq("app-events"), DateTime.now().minusMinutes(1)))
    client.expectMsgClass(classOf[Subscribed[_]])
    client.expectMsgClass(classOf[GetReplayResponse]).events should have size 1
  }

  it should "subscribe to command trail" in {
    val bus = system.actorOf(core.EventBus(emptyDirectory))
    val client = TestProbe()

    client.send(bus, SubscribeToCommandTrail(CommandDetails(CommandId("2")), CommandId("1"), Seq(classOf[CommandExecution])))

    bus ! CommandSentEvent(EventDetails(EventId.generate(), EventKey(), Seq(CommandId("1"))), DummyCommand(CommandDetails()))
    val commandSentNotification = client.expectMsgClass(classOf[EventDelivered])
    commandSentNotification.eventDetails.causedBy should be (Seq(CommandId("2")))
    commandSentNotification.event.eventDetails.causedBy should be (Seq(CommandId("1")))

    bus ! DummyProgress(EventDetails(EventId.generate(), EventKey(), Seq(CommandId("1"))))
    val progressNotification = client.expectMsgClass(classOf[EventDelivered])
    progressNotification.eventDetails.causedBy should be (Seq(CommandId("2")))
    progressNotification.event.eventDetails.causedBy should be (Seq(CommandId("1")))

    bus ! DummyProgress(EventDetails(EventId.generate(), EventKey(), Seq(CommandId("otherCommandId"))))
    client.expectNoMsg()

    bus ! DummySuccess(EventDetails(EventId.generate(), EventKey(), Seq(CommandId("1"))))
    client.expectMsgClass(classOf[EventDelivered])
    client.expectMsgClass(classOf[EventDeliveryComplete])
  }

  it should "serialize a snapshot" in {
    val string = EventBus.serializeSnapshots(Map(SnapshotEntryType(classOf[NodeState]) -> Snapshot(SnapshotEntryType(classOf[NodeState]), Map("1" -> NodeCreated))))
    println(s"string=$string")
  }

  private def emptyDirectory: File = {
    val dir = new File("target/easyrider")
    FileUtils.deleteDirectory(dir)
    dir.mkdirs()
    dir
  }

  case class DummyCommand(commandDetails: CommandDetails) extends Command
  case class DummyProgress(eventDetails: EventDetails) extends CommandExecution
  case class DummySuccess(eventDetails: EventDetails) extends Success
  val dummySubscribe = Subscribe(CommandDetails(), "all", classOf[NodeUpdatedEvent], EventKey())
  val dummyEvent = NodeUpdatedEvent(EventDetails(EventId("1"), EventKey(), Seq()), NodeId("nodeId"), NodeCreated)
}
