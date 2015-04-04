package easyrider.testing

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import easyrider.Api.{AuthenticateUser, Authentication}
import easyrider.{CommandDetails, CommandId}
import org.scalatest.{FlatSpecLike, Matchers}

class IntegrationTest() extends TestKit(ActorSystem()) with FlatSpecLike with Matchers  {
  "EasyRider" should "allow to manage applications" in {
    val probe = TestProbe()
    val easyRider = system.actorOf(EasyRiderProbe(probe.ref))

    //socket ! Send(TextFrame("{\"jsonClass\":\"easyrider.Api$ReAuthenticateUser\",\"username\":\"admin\",\"signature\":\"fbe7be693095f37eb01594c4127a7b5f0cc2ff91fc9bed2477b9dc52eaf53ca5e2b902809d128c3ad0d83b939b2e9fdf3d0d024e87da737cdef448d2d2c6c978\"}"))
    easyRider ! AuthenticateUser(CommandDetails(CommandId("1")), "admin", "test")
    println("got: " + probe.expectMsgClass(classOf[Authentication]))
  }
}
