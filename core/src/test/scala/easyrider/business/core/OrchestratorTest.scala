package easyrider.business.core

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.scalatest.{FlatSpecLike, Matchers}

class OrchestratorTest extends TestKit(ActorSystem()) with FlatSpecLike with Matchers {
  "Orchestrator" should "perform rolling release" in {
    fail("TODO")
  }
}
