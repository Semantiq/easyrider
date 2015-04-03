
package easyrider.business.core

import java.io.File

import akka.actor.{ActorSystem, PoisonPill, Terminated}
import akka.testkit.{TestKit, TestProbe}
import easyrider.Applications.{Application, ApplicationId, ApplicationUpdatedEvent}
import easyrider.Commands.{CommandExecution, Success}
import easyrider.Events._
import easyrider.Implicits._
import easyrider.Infrastructure.{NodeCreated, NodeState, NodeUpdatedEvent}
import easyrider._
import jdk.nashorn.internal.runtime.regexp.joni.constants.NodeStatus
import org.apache.commons.io.FileUtils
import org.scalatest._

class EventBusTest() extends TestKit(ActorSystem()) with FlatSpecLike with Matchers {
  val application = Application(ApplicationId("app"), Seq())

  "EventBus" should "send events to subscribers" in {
    val bus = system.actorOf(EventBus(emptyDirectory))

    val publisher = TestProbe()
    val subscriber = TestProbe()

    subscriber.send(bus, dummySubscribe)
    subscriber.expectMsgClass(classOf[Subscribed[_]])
    publisher.send(bus, dummyEvent)
    subscriber.expectMsgClass(classOf[NodeUpdatedEvent])
  }

  it should "not send events after un-subscribe" in {
    val bus = system.actorOf(EventBus(emptyDirectory))

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

  it should "recover snapshots from file" in {
    val directory = emptyDirectory
    val bus0 = system.actorOf(EventBus(directory))
    val client = TestProbe()
    client.send(bus0, ApplicationUpdatedEvent(EventDetails(EventId("3"), EventKey("app"), Seq(CommandId("3"))), CommandId("3"), snapshotUpdate = SnapshotUpdateDetails(SnapshotEntryType(classOf[Application]), application.id.eventKey, Some(application))))
    client.watch(bus0)
    client.send(bus0, PoisonPill)
    client.expectMsgClass(classOf[Terminated])
    val bus1 = system.actorOf(EventBus(directory))
    client.send(bus1, GetSnapshot(CommandDetails(), SnapshotEntryType(classOf[Application])))
    client.expectMsgClass(classOf[GetSnapshotResponse[Application]]).snapshot.entries.size should be (1)
  }

  it should "handle snapshot subscriptions" in {
    val bus = system.actorOf(EventBus(emptyDirectory))
    val client = TestProbe()

    client.send(bus, StartSnapshotSubscription(CommandDetails(CommandId("1")), SnapshotEntryType(classOf[NodeState])))
    val snapshot = client.expectMsgClass(classOf[SnapshotSubscriptionStarted[NodeState]])
    snapshot.executionOf should be (CommandId("1"))
    snapshot.snapshot.entryType should be (SnapshotEntryType(classOf[NodeState]))
    snapshot.snapshot.entries should be (Map())

    client.send(bus, NodeUpdatedEvent(EventDetails(EventId.generate(), EventKey("1"), Seq()), NodeId("1"), NodeCreated, SnapshotUpdateDetails(SnapshotEntryType(classOf[NodeStatus]), NodeId("1").eventKey, Some(NodeCreated))))
    val update = client.expectMsgClass(classOf[SnapshotUpdatedEvent[NodeState]])
    val updatedSnapshot = snapshot.snapshot.updatedWith(update.update)
    update.executionOf should be (CommandId("1"))
    updatedSnapshot.entries should be (Map("1" -> NodeCreated))

    client.send(bus, StopSnapshotSubscription(CommandDetails(), CommandId("1")))
    client.send(bus, NodeUpdatedEvent(EventDetails(EventId.generate(), EventKey("2"), Seq()), NodeId("2"), NodeCreated, SnapshotUpdateDetails(SnapshotEntryType(classOf[NodeStatus]), NodeId("1").eventKey, Some(NodeCreated))))
    client.expectNoMsg()
  }

  private def emptyDirectory: File = {
    val dir = new File("target/easyrider")
    FileUtils.deleteDirectory(dir)
    dir.mkdirs()
    dir
  }

  case class DummyCommand(commandDetails: CommandDetails) extends Command
  case class DummyProgress(eventDetails: EventDetails, executionOf: CommandId) extends CommandExecution
  case class DummySuccess(eventDetails: EventDetails, executionOf: CommandId, successMessage: String) extends Success
  val dummySubscribe = Subscribe(CommandDetails(), "all", classOf[NodeUpdatedEvent], EventKey())
  val dummyEvent = NodeUpdatedEvent(EventDetails(EventId("1"), EventKey(), Seq()), NodeId("nodeId"), NodeCreated, SnapshotUpdateDetails(SnapshotEntryType(classOf[NodeStatus]), NodeId("nodeId").eventKey, Some(NodeCreated)))
}
