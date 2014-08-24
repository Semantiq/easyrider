package easyrider.infrastructure.ssh

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{FlatSpecLike, Matchers}

class SshInfrastructureTest extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ImplicitSender {
  "SshAgentLessInfrastructure" should "return list of all nodes" in {
    val infrastructure = system.actorOf(SshInfrastructure())
  }
}
