package eu.semantiq.easyrider

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.ActorSystem
import org.scalatest.{BeforeAndAfterAll, Tag, FunSpecLike}
import java.io.File
import eu.semantiq.easyrider.builder.DummyGitRepository
import org.apache.commons.io.FileUtils
import org.scalatest.concurrent.Eventually._
import org.scalatest.Matchers._
import org.scalatest.time.{Seconds, Span}
import scala.concurrent.duration._

class LeadingActorTest extends TestKit(ActorSystem("LeadingActorTest")) with ImplicitSender with FunSpecLike with BeforeAndAfterAll {
  private val patience = PatienceConfig(timeout = Span(2, Seconds))

  override def afterAll() {
    system.shutdown()
  }

  it("should build & start configured applications at startup") {
    setup("buildAndStart")

    eventually {
      new File("target/marker_buildAndStart") should exist
    } (patience)
  }

  it("should build & start new applications after configuration update") {
    val (_, configuration, _, git) = setup("buildAndStartNewApps")

    configuration.update(Seq(
      Application("demo", git.gitURL, Map("FILE" -> markerFile("marker_buildAndStartNewApps"))),
      Application("new_app", git.gitURL, Map("FILE" -> markerFile("marker_new_app")))
    ))

    eventually {
      new File(markerFile("marker_new_app")) should exist
    } (patience)
  }

  it("should stop & remove old applications after configuration update") {
    val (_, configuration, _, _) = setup("buildAndStartNewApps")

    configuration.update(Seq())

    val marker = new File(markerFile("marker_buildAndStartNewApps"))
    marker.delete()
    Thread.sleep(2000)
    marker shouldNot exist
  }

  it("should propagate configuration changes to builders and supervisors", Tag("this")) {
    val (_, configuration, _, git) = setup("updateApps")

    eventually {
      new File(markerFile("marker_updateApps")) should exist
    } (patience)

    configuration.update(Seq(Application("demo", git.gitURL, Map("FILE" -> markerFile("marker_updated")))))

    eventually {
      new File(markerFile("marker_updated")) should exist
    } (patience)
  }

  private def setup(id: String) = {
    val git = new DummyGitRepository(s"LeadingActorTest.git.$id")
    git.updateFile("run.sh", FileUtils.readFileToString(new File("src/test/resources/createFilePackage/run.sh")))
    git.updateFile(".easyrider.json", FileUtils.readFileToString(new File("src/test/resources/createFilePackage/.easyrider.json")))
    val configuration = new DummyConfigFile(s"LeadingActorTest.$id")
    configuration.update(Seq(Application("demo", git.gitURL, Map("FILE" -> markerFile(s"marker_$id")))))
    val workingDirectory = new File(s"target/LeadingActorTest.$id")
    FileUtils.deleteDirectory(workingDirectory)
    FileUtils.deleteDirectory(markers)
    markers.mkdir()
    val leader = system.actorOf(LeadingActor(configuration.location, workingDirectory, configurationPollingInterval = 200.millis))
    (leader, configuration, workingDirectory, git)
  }

  private def markers = new File("target/markers")
  private def markerFile(name: String) = new File(markers, name).getAbsolutePath
}
