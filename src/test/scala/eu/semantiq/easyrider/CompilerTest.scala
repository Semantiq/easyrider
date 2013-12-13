package eu.semantiq.easyrider

import scala.concurrent.duration._
import akka.testkit.{TestProbe, ImplicitSender, TestKit}
import akka.actor.ActorSystem
import org.scalatest.{Matchers, FunSpecLike}

class CompilerTest extends TestKit(ActorSystem("CompilerTest")) with ImplicitSender with FunSpecLike with Matchers {
  it("should compile whenever working copy is available and notify success") {
    // given
    val probe = TestProbe()
    val workingCopy = new DummyWorkingCopy
    // when
    val compiler = system.actorOf(Compiler(probe.ref, workingCopy.location, Some("sh ok.sh")), "compiler-ok")
    // then
    probe.expectNoMsg(500.milliseconds)
    compiler ! AppSupervisor.WorkingCopyUpdated
    probe.expectMsg(200.milliseconds, Compiler.CompilationSuccessful)
  }
  it("should safely abort compilation if working copy is updated during compilation") { pending }
  it("should notify about compilation failures") { pending }
}
