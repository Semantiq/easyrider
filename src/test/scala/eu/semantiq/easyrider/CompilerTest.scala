package eu.semantiq.easyrider

import scala.concurrent.duration._
import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.ActorSystem
import org.scalatest.{Matchers, FunSpecLike}
import Compiler._

class CompilerTest extends TestKit(ActorSystem("CompilerTest")) with ImplicitSender with FunSpecLike with Matchers {
  val workingCopy = new DummyWorkingCopy

  it("should compile whenever working copy is available and notify success") {
    val compiler = system.actorOf(Compiler(testActor, workingCopy.location, 2.seconds), "compiler-ok")

    expectNoMsg(500.milliseconds)
    compiler ! Compile(Some("sh ok.sh"))
    expectMsg(200.milliseconds, Compiler.CompilationSuccessful)
  }
  it("should safely abort compilation if working copy is updated during compilation") {
    val compiler = system.actorOf(Compiler(testActor, workingCopy.location, 2.seconds), "compiler-restart")

    compiler ! Compile(Some("sh 1sec.sh"))
    compiler ! Compile(Some("sh 1sec.sh"))
    expectMsg(CompilationSuccessful)
    expectNoMsg(2.seconds)
  }
  it("should notify about compilation failures") {
    val compiler = system.actorOf(Compiler(testActor, workingCopy.location, 2.seconds))

    compiler ! Compile(Some("sh failing.sh"))
    expectMsg(CompilationFailure)
  }
  it("should notify about compilation failure in case of timeout") {
    val compiler = system.actorOf(Compiler(testActor, workingCopy.location, 500.milliseconds))

    compiler ! Compile(Some("sh 10sec.sh"))
    expectMsg(CompilationFailure)
  }
}
