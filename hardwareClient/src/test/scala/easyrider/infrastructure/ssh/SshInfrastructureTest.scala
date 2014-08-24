package easyrider.infrastructure.ssh

import akka.actor.ActorSystem
import akka.testkit.{TestProbe, ImplicitSender, TestKit}
import org.scalatest.{FlatSpecLike, Matchers}

class SshInfrastructureTest extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ImplicitSender {
  "SshAgentLessInfrastructure" should "return list of all nodes" in {
    val eventBus = TestProbe()
    val infrastructure = system.actorOf(SshInfrastructure(SshNodeAgent(eventBus.ref)))
  }
}
