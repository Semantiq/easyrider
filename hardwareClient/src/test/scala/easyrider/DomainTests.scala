package easyrider

import org.scalatest.{FlatSpec, Matchers}

class DomainTest extends FlatSpec with Matchers {
  "EventKey" should "contain more specific EventKeys" in {
    EventKey(Seq("a", "b")) contains EventKey(Seq("a", "b", "c")) should be (true)
    EventKey(Seq("a", "b")) contains EventKey(Seq("a")) should be (false)
    EventKey(Seq()) contains EventKey(Seq("anything", "matches")) should be (true)
  }
}
