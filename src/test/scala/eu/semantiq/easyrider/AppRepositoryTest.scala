package eu.semantiq.easyrider

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.ActorSystem
import org.scalatest.Matchers._
import org.scalatest.FunSpecLike
import java.io.File
import eu.semantiq.easyrider.AppRepository.{PackageRef, GetVersion, VersionAvailable}
import scala.concurrent.duration._
import akka.pattern.AskSupport
import akka.util.Timeout
import scala.concurrent.Await.result

class AppRepositoryTest extends TestKit(ActorSystem("AppRepositoryTest")) with ImplicitSender with FunSpecLike with AskSupport {
  private val storageFolder = new File("target/AppRepositoryTest.appRepo")
  private val downloadFolder = new File("target/AppRepositoryTest.downloaded")
  private val timeoutDuration = 1.second
  private implicit val timeout = Timeout(timeoutDuration)

  it("deploy, notify and download scenario") {
    val repo = system.actorOf(AppRepository(storageFolder))
    val pkg = AppRepository.PackageRef.fromFolder(new File("src/test/resources/samplePackage"))
    system.eventStream.subscribe(testActor, classOf[VersionAvailable])

    repo ! AppRepository.DeployVersion("sample", "1", pkg)
    val version = receiveOne(timeoutDuration).asInstanceOf[VersionAvailable]
    val packageRef = repo ? GetVersion(version.app, version.version)
    result(packageRef, timeoutDuration).asInstanceOf[PackageRef].extractTo(downloadFolder)

    (new File(downloadFolder, "run.sh").exists()) should be (true)
  }
}
