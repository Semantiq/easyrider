package easyrider.business

import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{FlatSpecLike, Matchers}

abstract class EasyriderTest(val easyrider: Easyrider) extends TestKit(easyrider.actorSystem) with FlatSpecLike with Matchers with ImplicitSender {
}
