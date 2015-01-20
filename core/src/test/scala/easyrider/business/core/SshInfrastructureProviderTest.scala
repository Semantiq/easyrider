package easyrider.business.core

import java.net.URL

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import easyrider.business.ssh.{BuiltInPackageUpload, SshSession, SshNodeAgent, SshInfrastructureProvider}
import org.scalatest.{FlatSpecLike, Matchers}

class SshInfrastructureProviderTest extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ImplicitSender {
  "SshAgentLessInfrastructure" should "return list of all nodes" in {
    val eventBus = TestProbe()
    val repository = TestProbe()
    val sshSession = TestProbe()
    val easyRiderUrl = new URL("http://localhost:8082")
    val infrastructure = system.actorOf(SshInfrastructureProvider(eventBus.ref, SshNodeAgent(eventBus.ref, easyRiderUrl, sshSession.ref, BuiltInPackageUpload(sshSession.ref, repository.ref))))
  }
}
