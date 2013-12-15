package eu.semantiq.easyrider

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.ActorSystem
import org.scalatest.{Matchers, FunSpecLike}
import eu.semantiq.easyrider.AppSupervisor._
import scala.concurrent.duration._
import org.apache.commons.io.FileUtils
import java.io.File

class AppSupervisorTest extends TestKit(ActorSystem("AppSupervisorTest")) with ImplicitSender with FunSpecLike with Matchers {
  it("should clone, compile and run when receives configuration") {
    val id = "initial-startup"
    val git = gitRepository(id)
    FileUtils.deleteDirectory(new File(s"working/$id"))
    val supervisor = system.actorOf(AppSupervisor(new File("working")), id)
    system.eventStream.subscribe(testActor, classOf[AppLifecycleEvent])

    supervisor ! ConfigurationUpdated(Application(id, git.gitURL, Commands("sh run.sh", Some("sh compile.sh"))))
    expectMsgClass(classOf[Updated])
    expectMsgClass(classOf[Compiled])
    expectMsgClass(classOf[Started])
    expectNoMsg()
  }

  it("should should recompile and start new version of application, upon a change") {
    val id = "restart-on-update"
    val git = gitRepository(id)
    FileUtils.deleteDirectory(new File(s"working/$id"))
    val supervisor = system.actorOf(AppSupervisor(new File("working")), id)
    system.eventStream.subscribe(testActor, classOf[AppLifecycleEvent])

    val initialConfiguration: Application = Application(id, git.gitURL, Commands("sh run.sh", Some("sh compile.sh")))
    supervisor ! ConfigurationUpdated(initialConfiguration)
    expectMsgClass(classOf[Updated])
    expectMsgClass(classOf[Compiled])
    expectMsgClass(classOf[Started])
    expectNoMsg()
    git.updateFile("compile.sh", """echo world > run.sh""")
    expectMsgClass(40.seconds, classOf[Updated])
    expectMsgClass(classOf[Compiled])
    expectMsgClass(classOf[Started])
    expectNoMsg()
  }

  it("should keep the application running until new version is compiled successfully") {
    pending
  }

  private def gitRepository(id: String) = {
    val git = new DummyGitRepository(id)
    git.updateFile("compile.sh",
      """#!/bin/sh
        |echo "echo hello" > run.sh
      """.stripMargin)
    git
  }
}
