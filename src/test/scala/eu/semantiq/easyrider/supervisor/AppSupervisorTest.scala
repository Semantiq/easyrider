package eu.semantiq.easyrider.supervisor

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.{ActorRef, ActorSystem}
import org.scalatest.{Matchers, FunSpecLike}
import eu.semantiq.easyrider._
import java.io.File
import org.apache.commons.io.FileUtils
import org.scalatest.concurrent.Eventually._
import scala.concurrent.duration._

class AppSupervisorTest extends TestKit(ActorSystem("AppSupervisorTest")) with ImplicitSender with FunSpecLike with Matchers {
  import AppSupervisor._

  it("should start application after receiving VersionAvailable and ConfigurationUpdated") {
    val (supervisor, appName, workingDirectory) = setup("simpleCase")

    supervisor ! ConfigurationUpdated(Map("FILE" -> "marker"))
    expectMsg(AppRepository.GetVersionAvailable(appName))
    supervisor ! AppRepository.VersionAvailable(appName, "1")
    expectMsg(AppRepository.GetVersion(appName, "1"))
    supervisor ! AppRepository.GetVersionResponse(appName, "1", createTestPackage("createFilePackage"), defaultMetadata)

    eventually {
      new File(workingDirectory, "1/marker") should exist
    }
  }

  it("should start new version of application upon VersionAvailable") {
    val (supervisor, appName, workingDirectory) = setup("packageUpdate")

    supervisor ! ConfigurationUpdated(Map("FILE" -> "marker"))

    expectMsg(AppRepository.GetVersionAvailable(appName))
    supervisor ! AppRepository.VersionAvailable(appName, "1")

    expectMsg(AppRepository.GetVersion(appName, "1"))
    supervisor ! AppRepository.GetVersionResponse(appName, "1", createTestPackage("createFilePackage"), defaultMetadata)
    eventually {
      new File(workingDirectory, "1/marker") should exist
    }

    system.eventStream.publish(AppRepository.VersionAvailable(appName, "2"))

    expectMsg(AppRepository.GetVersion(appName, "2"))
    supervisor ! AppRepository.GetVersionResponse(appName, "2", createTestPackage("alternativeCreateFilePackage"), defaultMetadata)
    eventually {
      new File(workingDirectory, "2/alternative_marker")
    }
  }

  it("should restart with new settings upon ConfigurationUpdated") {
    val (supervisor, appName, workingDirectory) = setup("configUpdate")

    supervisor ! ConfigurationUpdated(Map("FILE" -> "marker"))

    expectMsg(AppRepository.GetVersionAvailable(appName))
    supervisor ! AppRepository.VersionAvailable(appName, "1")

    expectMsg(AppRepository.GetVersion(appName, "1"))
    supervisor ! AppRepository.GetVersionResponse(appName, "1", createTestPackage("createFilePackage"), defaultMetadata)
    eventually {
      new File(workingDirectory, "1/marker") should exist
    }

    supervisor ! ConfigurationUpdated(Map("FILE" -> "updated_marker"))
    eventually {
      new File(workingDirectory, "1/updated_marker") should exist
    }
  }

  private def setup(id: String) = {
    val workingDirectory = new File(s"target/AppSupervisorTest.$id")
    FileUtils.deleteDirectory(workingDirectory)
    val supervisor: ActorRef = system.actorOf(AppSupervisor(id, testActor, workingDirectory), id)
    (supervisor, id, workingDirectory)
  }

  private def createTestPackage(name: String) = AppRepository.PackageRef.fromFolder(new File(s"src/test/resources/$name"))
  private def defaultMetadata = PackageMetadata(Compilation(None, "."), Running("sh run.sh"))
}
