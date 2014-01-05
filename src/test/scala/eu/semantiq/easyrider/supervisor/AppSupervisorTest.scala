package eu.semantiq.easyrider.supervisor

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.{ActorRef, ActorSystem}
import org.scalatest.{Tag, Matchers, FunSpecLike}
import eu.semantiq.easyrider._
import java.io.File
import org.apache.commons.io.FileUtils
import org.scalatest.concurrent.Eventually._

class AppSupervisorTest extends TestKit(ActorSystem("AppSupervisorTest")) with ImplicitSender with FunSpecLike with Matchers {
  import AppSupervisor._

  it("should start application after receiving VersionAvailable and ConfigurationUpdated") {
    val (supervisor, appName, workingDirectory) = setup("simpleCase")
    givenConfigurationAndPackageVersion(supervisor, appName)
    expectMsg(AppSupervisor.Started(appName, "1"))
    fileShouldEventuallyExist(workingDirectory, "1/marker")
  }

  it("should start new version of application upon VersionAvailable") {
    val (supervisor, appName, workingDirectory) = setup("packageUpdate")
    givenConfigurationAndPackageVersion(supervisor, appName)

    expectMsg(AppSupervisor.Started(appName, "1"))
    fileShouldEventuallyExist(workingDirectory, "1/marker")

    system.eventStream.publish(AppRepository.VersionAvailable(appName, "2"))
    respondToGetVersion(appName, supervisor, "2", "alternativeCreateFilePackage")

    expectMsg(AppSupervisor.Started(appName, "2"))
    fileShouldEventuallyExist(workingDirectory, "2/alternative_marker")
  }

  it("should restart with new settings upon ConfigurationUpdated") {
    val (supervisor, appName, workingDirectory) = setup("configUpdate")
    givenConfigurationAndPackageVersion(supervisor, appName)

    expectMsg(AppSupervisor.Started(appName, "1"))
    fileShouldEventuallyExist(workingDirectory, "1/marker")

    supervisor ! ConfigurationUpdated(Map("FILE" -> "updated_marker"))
    expectMsg(AppSupervisor.Started(appName, "1"))
    fileShouldEventuallyExist(workingDirectory, "1/updated_marker")
  }

  it("should start when crashed") {
    val (supervisor, appName, workingDirectory) = setup("startAfterCrash")
    givenConfigurationAndPackageVersion(supervisor, appName, crash = true)
    expectMsg(AppSupervisor.Started(appName, "1"))
    expectMsg(AppSupervisor.Crashed(appName, 0))
    new File(workingDirectory, "1/marker").delete()

    supervisor ! Start
    expectMsg(AppSupervisor.Started(appName, "1"))
    expectMsg(AppSupervisor.Crashed(appName, 0))

    fileShouldEventuallyExist(workingDirectory, "1/marker")
  }

  it("should try to start again on VersionAvailable when crashed") {
    val (supervisor, appName, workingDirectory) = setup("packageUpdateAfterCrash")
    givenConfigurationAndPackageVersion(supervisor, appName, crash = true)

    expectMsg(AppSupervisor.Started(appName, "1"))
    expectMsg(AppSupervisor.Crashed(appName, 0))
    new File(workingDirectory, "1/marker") should exist

    system.eventStream.publish(AppRepository.VersionAvailable(appName, "2"))

    respondToGetVersion(appName, supervisor, "2", "alternativeCreateFilePackage")

    expectMsg(AppSupervisor.Started(appName, "2"))
    new File(workingDirectory, "2/alternative_marker")
  }

  it("should try to start again on ConfigurationUpdated when crashed", Tag("this")) {
    val (supervisor, appName, workingDirectory) = setup("configUpdateAfterCrash")
    givenConfigurationAndPackageVersion(supervisor, appName, crash = true)

    expectMsg(AppSupervisor.Started(appName, "1"))
    expectMsg(AppSupervisor.Crashed(appName, 0))
    new File(workingDirectory, "1/marker") should exist

    supervisor ! ConfigurationUpdated(Map("FILE" -> "updated_marker"))
    expectMsg(AppSupervisor.Started(appName, "1"))
    fileShouldEventuallyExist(workingDirectory, "1/updated_marker")
  }

  it("should stop and start on request") {
    pending
  }

  it("should restart on request") {
    pending
  }

  private def setup(id: String) = {
    system.eventStream.subscribe(testActor, classOf[AppSupervisor.AppLifecycleEvent])
    val workingDirectory = new File(s"target/AppSupervisorTest.$id")
    FileUtils.deleteDirectory(workingDirectory)
    val supervisor: ActorRef = system.actorOf(AppSupervisor(id, testActor, workingDirectory), id)
    (supervisor, id, workingDirectory)
  }

  private def respondToGetVersionAvailable(appName: String, supervisor: ActorRef) {
    expectMsg(AppRepository.GetVersionAvailable(appName))
    supervisor ! AppRepository.VersionAvailable(appName, "1")
  }

  private def respondToGetVersion(appName: String, supervisor: ActorRef, version: String, testPackage: String) {
    expectMsg(AppRepository.GetVersion(appName, version))
    supervisor ! AppRepository.GetVersionResponse(appName, version, createTestPackage(testPackage), defaultMetadata)
  }

  private def givenConfigurationAndPackageVersion(supervisor: ActorRef, appName: String, crash: Boolean = false) {
    supervisor ! ConfigurationUpdated(Map("FILE" -> "marker", "CRASH" -> crash.toString))
    respondToGetVersionAvailable(appName, supervisor)
    respondToGetVersion(appName, supervisor, "1", "createFilePackage")
  }

  private def fileShouldEventuallyExist(workingDirectory: File, fileName: String) {
    eventually {
      new File(workingDirectory, fileName) should exist
    }
  }

  private def createTestPackage(name: String) = AppRepository.PackageRef.fromFolder(new File(s"src/test/resources/$name"))
  private def defaultMetadata = PackageMetadata(Compilation(None, "."), Running("sh run.sh"))
}
