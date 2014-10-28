package easyrider

import org.scalatest.{FlatSpec, Matchers}

class DomainTest extends FlatSpec with Matchers {
  "EventKey" should "contain more specific EventKeys" in {
    EventKey("a", "b") contains EventKey("a", "b", "c") should be (true)
    EventKey("a", "b") contains EventKey("a") should be (false)
    EventKey() contains EventKey("anything", "matches") should be (true)
  }
}
