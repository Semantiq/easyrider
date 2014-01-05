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
    givenConfiguredAndStarted(supervisor, appName)
    fileShouldEventuallyExist(workingDirectory, "1/marker")
  }

  it("should start new version of application upon VersionAvailable") {
    val (supervisor, appName, workingDirectory) = setup("packageUpdate")
    givenConfiguredAndStarted(supervisor, appName)

    fileShouldEventuallyExist(workingDirectory, "1/marker")

    system.eventStream.publish(AppRepository.VersionAvailable(appName, "2"))
    respondToGetVersion(appName, supervisor, "2", "alternativeCreateFilePackage")

    expectMsg(AppSupervisor.Started(appName, "2"))
    fileShouldEventuallyExist(workingDirectory, "2/alternative_marker")
  }

  it("should restart with new settings upon ConfigurationUpdated") {
    val (supervisor, appName, workingDirectory) = setup("configUpdate")
    givenConfiguredAndStarted(supervisor, appName)

    fileShouldEventuallyExist(workingDirectory, "1/marker")

    supervisor ! ConfigurationUpdated(Map("FILE" -> "updated_marker"))
    expectMsg(AppSupervisor.Started(appName, "1"))
    fileShouldEventuallyExist(workingDirectory, "1/updated_marker")
  }

  it("should start when crashed") {
    val (supervisor, appName, workingDirectory) = setup("startAfterCrash")
    givenConfiguredAndStarted(supervisor, appName, crash = true)
    expectMsg(AppSupervisor.Crashed(appName, 0))
    new File(workingDirectory, "1/marker").delete()

    startAndWaitForConfirmation(appName)
    expectMsg(AppSupervisor.Crashed(appName, 0))

    fileShouldEventuallyExist(workingDirectory, "1/marker")
  }

  it("should try to start again on VersionAvailable when crashed") {
    val (supervisor, appName, workingDirectory) = setup("packageUpdateAfterCrash")
    givenConfiguredAndStarted(supervisor, appName, crash = true)

    expectMsg(AppSupervisor.Crashed(appName, 0))
    new File(workingDirectory, "1/marker") should exist

    system.eventStream.publish(AppRepository.VersionAvailable(appName, "2"))

    respondToGetVersion(appName, supervisor, "2", "alternativeCreateFilePackage")

    expectMsg(AppSupervisor.Started(appName, "2"))
    new File(workingDirectory, "2/alternative_marker")
  }

  it("should try to start again on ConfigurationUpdated when crashed", Tag("this")) {
    val (supervisor, appName, workingDirectory) = setup("configUpdateAfterCrash")
    givenConfiguredAndStarted(supervisor, appName, crash = true)

    expectMsg(AppSupervisor.Crashed(appName, 0))
    new File(workingDirectory, "1/marker") should exist

    supervisor ! ConfigurationUpdated(Map("FILE" -> "updated_marker"))
    expectMsg(AppSupervisor.Started(appName, "1"))
    fileShouldEventuallyExist(workingDirectory, "1/updated_marker")
  }

  it("should stop and start on request") {
    val (supervisor, appName, workingDirectory) = setup("stopAndStart")
    val markerFile = new File(workingDirectory, "1/marker")
    givenConfiguredAndStarted(supervisor, appName)

    stopAndWaitForConfirmation(appName)

    markerFile.delete()
    Thread.sleep(2000)
    markerFile shouldNot exist

    startAndWaitForConfirmation(appName)
    fileShouldEventuallyExist(workingDirectory, "1/marker")
  }

  it("should record configuration changes and package updates when stopped") {
    val (supervisor, appName, workingDirectory) = setup("stopUpdateAndStart")
    givenConfiguredAndStarted(supervisor, appName)
    stopAndWaitForConfirmation(appName)

    system.eventStream.publish(AppRepository.VersionAvailable(appName, "2"))
    respondToGetVersion(appName, supervisor, "2", "alternativeCreateFilePackage")

    supervisor ! ConfigurationUpdated(Map("FILE" -> "updated_marker"))

    startAndWaitForConfirmation(appName, version = "2")
    fileShouldEventuallyExist(workingDirectory, "2/alternative_updated_marker")
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

  private def givenConfiguredAndStarted(supervisor: ActorRef, appName: String, crash: Boolean = false) {
    supervisor ! ConfigurationUpdated(Map("FILE" -> "marker", "CRASH" -> crash.toString))
    respondToGetVersionAvailable(appName, supervisor)
    respondToGetVersion(appName, supervisor, "1", "createFilePackage")
    expectMsg(AppSupervisor.Started(appName, "1"))
  }

  private def fileShouldEventuallyExist(workingDirectory: File, fileName: String) {
    eventually {
      new File(workingDirectory, fileName) should exist
    }
  }

  private def startAndWaitForConfirmation(appName: String, version: String = "1") {
    system.eventStream.publish(Start(appName))
    expectMsg(AppSupervisor.Started(appName, version))
  }

  private def stopAndWaitForConfirmation(appName: String) {
    system.eventStream.publish(Stop(appName))
    expectMsg(AppSupervisor.Stopped(appName))
  }

  private def createTestPackage(name: String) = AppRepository.PackageRef.fromFolder(new File(s"src/test/resources/$name"))
  private def defaultMetadata = PackageMetadata(Compilation(None, "."), Running("sh run.sh"))
}
