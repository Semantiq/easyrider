package easyrider.business.core

import java.net.URL

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{FlatSpecLike, Matchers}

class SshInfrastructureTest extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ImplicitSender {
  "SshAgentLessInfrastructure" should "return list of all nodes" in {
    val eventBus = TestProbe()
    val repository = TestProbe()
    val easyRiderUrl = new URL("http://localhost:8081")
    val infrastructure = system.actorOf(SshInfrastructure(eventBus.ref, SshNodeAgent(eventBus.ref, easyRiderUrl, SshSession(eventBus.ref, repository.ref))))
  }
}
