package eu.semantiq.easyrider

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.ActorSystem
import org.scalatest.{Matchers, FunSpecLike}
import eu.semantiq.easyrider.AppSupervisor._
import scala.concurrent.duration._
import org.apache.commons.io.FileUtils
import java.io.File
import eu.semantiq.easyrider.builder.DummyGitRepository

class AppSupervisorTest extends TestKit(ActorSystem("AppSupervisorTest")) with ImplicitSender with FunSpecLike with Matchers {
  it("should clone, compile and run when receives configuration") {
    val (git, supervisor) = setup("initial-startup")

    supervisor ! ConfigurationUpdated(Application("initial-startup", git.gitURL, Compilation(Some("sh compile.sh"), "."), Running("sh run.sh")))
    expectMsgClass(classOf[Updated])
    expectMsgClass(classOf[Compiled])
    expectMsgClass(classOf[Started])
    expectNoMsg(200.milliseconds)
  }

  it("should should recompile and start new version of application, upon a change") {
    val (git, supervisor) = setup("restart-on-update")

    supervisor ! ConfigurationUpdated(Application("restart-on-update", git.gitURL, Compilation(Some("sh compile.sh"), "."), Running("sh run.sh")))
    expectMsgClass(classOf[Updated])
    expectMsgClass(classOf[Compiled])
    expectMsgClass(classOf[Started])
    expectNoMsg(200.milliseconds)
    git.updateFile("compile.sh", """echo world > run.sh""")
    expectMsgClass(classOf[Updated])
    expectMsgClass(classOf[Compiled])
    expectMsgClass(classOf[Started])
    expectNoMsg(200.milliseconds)
  }

  it("should keep the application running until new version is compiled successfully") {
    val (git, supervisor) = setup("restart-on-successful-compilation")

    supervisor ! ConfigurationUpdated(Application("restart-on-successful-compilation", git.gitURL, Compilation(Some("sh compile.sh"), "."), Running("sh run.sh")))
    expectMsgClass(classOf[Updated])
    expectMsgClass(classOf[Compiled])
    expectMsgClass(classOf[Started])
    expectNoMsg(200.milliseconds)
    git.updateFile("compile.sh", """exit 1""")
    expectMsgClass(classOf[Updated])
    expectNoMsg(200.milliseconds)
    git.updateFile("compile.sh", """echo world > run.sh""")
    expectMsgClass(classOf[Updated])
    expectMsgClass(classOf[Compiled])
    expectMsgClass(classOf[Started])
    expectNoMsg(200.milliseconds)
  }

  private def gitRepository(id: String) = {
    val git = new DummyGitRepository(id)
    git.updateFile("compile.sh",
      """#!/bin/sh
        |echo "echo hello" > run.sh
      """.stripMargin)
    git
  }

  private def setup(id: String) = {
    val git = gitRepository(id)
    val workingDirectory = new File(s"working/$id")
    FileUtils.deleteDirectory(workingDirectory)
    val supervisor = system.actorOf(AppSupervisor(workingDirectory, 50.milliseconds, 2.seconds), id)
    system.eventStream.subscribe(testActor, classOf[AppLifecycleEvent])
    (git, supervisor)
  }
}
