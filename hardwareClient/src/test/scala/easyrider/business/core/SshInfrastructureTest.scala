package easyrider.business.core

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{FlatSpecLike, Matchers}

class SshInfrastructureTest extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ImplicitSender {
  "SshAgentLessInfrastructure" should "return list of all nodes" in {
    val eventBus = TestProbe()
    val infrastructure = system.actorOf(SshInfrastructure(eventBus.ref, SshNodeAgent(eventBus.ref)))
  }
}
