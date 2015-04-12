package easyrider.testing

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import easyrider.Api.{AuthenticateUser, Authentication}
import easyrider.Applications._
import easyrider.Commands.{CommandExecution, Success}
import easyrider.Infrastructure.RemoveContainer
import easyrider.Nodes.CreateNode
import easyrider._
import org.scalatest.{FlatSpecLike, Matchers}

class IntegrationTest() extends TestKit(ActorSystem()) with FlatSpecLike with Matchers {
  "EasyRider" should "allow to manage applications" in {
    val (probe, easyRider) = givenConnectedAndAuthenticated()
    easyRider ! RemoveStage(CommandDetails(CommandId("prep-1")), StageId(ApplicationId("webapp"), "dev"))
    probe.expectMsgClass(classOf[CommandExecution]).executionOf shouldBe CommandId("prep-1")
    easyRider ! RemoveApplication(CommandDetails(CommandId("prep-2")), ApplicationId("webapp"))
    probe.expectMsgClass(classOf[CommandExecution]).executionOf shouldBe CommandId("prep-2")
    easyRider ! RemoveContainer(CommandDetails(CommandId("prep-3")), ContainerId(StageId(ApplicationId("webapp"), "dev"), "local0"), force = true)
    probe.expectMsgClass(classOf[CommandExecution]).executionOf shouldBe CommandId("prep-3")

    easyRider ! CreateApplication(CommandDetails(CommandId("2")), Application(ApplicationId("webapp"), Seq()))
    probe.expectMsgClass(classOf[Success]).executionOf shouldBe CommandId("2")
    easyRider ! CreateStage(CommandDetails(CommandId("3")), Stage(StageId(ApplicationId("webapp"), "dev"), Seq()))
    probe.expectMsgClass(classOf[Success]).executionOf shouldBe CommandId("3")
    easyRider ! CreateNode(CommandDetails(CommandId("4")), NodeConfiguration(NodeId("local0"), "builtin", Seq(
      Property("builtin", "host", "localhost"),
      Property("builtin", "login", "test"),
      Property("builtin", "password", "test")
    )))
    probe.expectMsgClass(classOf[Success])
  }

  private def givenConnectedAndAuthenticated(): (TestProbe, ActorRef) = {
    val probe = TestProbe()
    val easyRider = system.actorOf(EasyRiderProbe(probe.ref))

    easyRider ! AuthenticateUser(CommandDetails(CommandId("1")), "admin", "test")
    probe.expectMsgClass(classOf[Authentication])
    (probe, easyRider)
  }
}
