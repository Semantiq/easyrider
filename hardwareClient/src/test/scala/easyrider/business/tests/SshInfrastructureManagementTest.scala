package easyrider.business.tests

import akka.testkit.TestProbe
import easyrider.Api.{AuthenticateUser, Authentication}
import easyrider.{EventKey, CommandId}
import easyrider.Components.ComponentCommand
import easyrider.Events.{Subscribed, Subscribe}
import easyrider.Infrastructure.NodeUpdatedEvent
import easyrider.business.{Easyrider, EasyriderTest}
import easyrider.infrastructure.ssh.SshInfrastructurePlugin
import easyrider.Implicits._

class SshInfrastructureManagementTest extends EasyriderTest(new Easyrider(8081)) {
  "EasyRider" should "allow to add ssh host" in {
    val client = TestProbe()
    val api = easyrider.actorSystem.actorOf(easyrider.core.apiFactory(client.ref))

    client.send(api, AuthenticateUser())
    client.expectMsgClass(classOf[Authentication])

    client.send(api, Subscribe(CommandId.generate(), "nodeEvents", classOf[NodeUpdatedEvent], EventKey()))
    client.expectMsgClass(classOf[Subscribed[_]])

    client.send(api, ComponentCommand(CommandId.generate(), SshInfrastructurePlugin.componentId, Map(
      "type" -> "NodeConfiguration",
      "id" -> "nodeA",
      "host" -> "localhost",
      "port" -> "22",
      "login" -> "test",
      "password" -> "test")))
    client.expectMsgClass(classOf[NodeUpdatedEvent])
  }

  it should "manage containers on ssh host" in {

  }

  it should "deploy application packages on a container on ssh host" in {

  }
}
