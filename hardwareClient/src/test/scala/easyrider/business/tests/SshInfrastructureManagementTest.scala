package easyrider.business.tests

import java.io.File

import akka.testkit.TestProbe
import easyrider.Api.{AuthenticateUser, Authentication}
import easyrider.Events.{Subscribe, Subscribed}
import easyrider.Implicits._
import easyrider.Infrastructure.{NodeId, NodeUpdatedEvent}
import easyrider.SshInfrastructure.NodeConfiguration
import easyrider.business.{Easyrider, EasyriderTest}
import easyrider.{CommandId, EventKey, SshInfrastructure}

class SshInfrastructureManagementTest extends EasyriderTest(new Easyrider(8081, new File("target/easyrider"))) {
  "EasyRider" should "allow to add ssh host" in {
    val client = TestProbe()
    val api = easyrider.actorSystem.actorOf(easyrider.core.apiFactory(client.ref))

    client.send(api, AuthenticateUser())
    client.expectMsgClass(classOf[Authentication])

    client.send(api, Subscribe(CommandId.generate(), "nodeEvents", classOf[NodeUpdatedEvent], EventKey()))
    client.expectMsgClass(classOf[Subscribed[_]])

    client.send(api, SshInfrastructure.CreateNode(CommandId.generate(), NodeConfiguration(NodeId("nodeA"), "localhost", 22, "test", "test")))
    client.expectMsgClass(classOf[NodeUpdatedEvent])
  }

  it should "manage containers on ssh host" in {

  }

  it should "deploy application packages on a container on ssh host" in {

  }
}
