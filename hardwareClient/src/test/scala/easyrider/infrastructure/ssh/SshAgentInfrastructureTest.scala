package easyrider.infrastructure.ssh

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{FlatSpecLike, Matchers}

class SshAgentInfrastructureTest extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ImplicitSender {
  "SshAgentLessInfrastructure" should "return list of all nodes" in {
    val infrastructure = system.actorOf(SshAgentInfrastructure())
  }
}
