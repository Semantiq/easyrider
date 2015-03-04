package easyrider.builtin

import java.net.URL

import akka.actor.ActorSystem
import akka.testkit.{TestProbe, ImplicitSender, TestKit}
import org.scalatest.{Matchers, FlatSpecLike}

class ContainersTest extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ImplicitSender {
  "SshAgentLessInfrastructure" should "return list of all nodes" in {
    val eventBus = TestProbe()
    val repository = TestProbe()
    val sshSession = TestProbe()
    val easyRiderUrl = new URL("http://localhost:8082")
    val infrastructure = system.actorOf(Containers(eventBus.ref, ContainerAgent(eventBus.ref, easyRiderUrl, sshSession.ref, BuiltInPackageUpload(sshSession.ref, repository.ref))))
  }
}
