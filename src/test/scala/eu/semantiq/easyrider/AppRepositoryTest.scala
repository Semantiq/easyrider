package eu.semantiq.easyrider

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.ActorSystem
import org.scalatest.Matchers._
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.FunSpecLike
import java.io.File
import eu.semantiq.easyrider.AppRepository.{GetVersionResponse, GetVersion, VersionAvailable}
import scala.concurrent.duration._

class AppRepositoryTest extends TestKit(ActorSystem("AppRepositoryTest")) with ImplicitSender with FunSpecLike {
  private val storageFolder = new File("target/AppRepositoryTest.appRepo")
  private val downloadFolder = new File("target/AppRepositoryTest.downloaded")
  private val timeoutDuration = 1.second

  it("deploy, notify and download scenario") {
    val repo = system.actorOf(AppRepository(storageFolder))
    val pkg = AppRepository.PackageRef.fromFolder(new File("src/test/resources/samplePackage"))
    system.eventStream.subscribe(testActor, classOf[VersionAvailable])

    repo ! AppRepository.DeployVersion("sample", "1", pkg, PackageMetadata(Compilation(None, "not relevant"), Running("sh run.sh")))
    val version = receiveOne(timeoutDuration).asInstanceOf[VersionAvailable]
    repo ! GetVersion(version.app, version.version)
    val response = receiveOne(timeoutDuration).asInstanceOf[GetVersionResponse]
    response.packageRef.extractTo(downloadFolder)

    response.packageMetadata.running should be (Running("sh run.sh"))
    new File(downloadFolder, "run.sh") should exist
  }

  private def exist: Matcher[File] = new Matcher[File] {
    def apply(left: File): MatchResult = {
      val matches = left.exists()
      MatchResult(matches, s"file ${left.getName} doesn't exist", s"file ${left.getName} exists")
    }
  }
}
