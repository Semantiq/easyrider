package eu.semantiq.easyrider.builder

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.ActorSystem
import org.scalatest.{Matchers, FunSpecLike}
import org.apache.commons.io.FileUtils
import java.io.File
import scala.concurrent.duration._
import eu.semantiq.easyrider.builder.GitWorkingCopy.WorkingCopyUpdated

class GitWorkingCopyTest extends TestKit(ActorSystem("GitWorkingCopyTest")) with ImplicitSender with FunSpecLike with Matchers {
  it("should clone the repository at startup") {
    val (repo, gitWorkingCopy) = setup("cloneTest")

    gitWorkingCopy ! GitWorkingCopy.ConfigurationUpdated(repo.gitURL)
    expectMsgClass(classOf[WorkingCopyUpdated])
  }

  it("should poll for incoming changes") {
    val (repo, gitWorkingCopy) = setup("pollingTest")

    gitWorkingCopy ! GitWorkingCopy.ConfigurationUpdated(repo.gitURL)
    expectMsgClass(classOf[WorkingCopyUpdated])
    expectNoMsg(200.milliseconds)
    repo.updateFile("anything", "new content")
    expectMsgClass(classOf[WorkingCopyUpdated])
    expectNoMsg(200.milliseconds)
  }

  private def setup(id: String) = {
    val repo = new DummyGitRepository(id)
    val folder = notExistingFolder(s"target/${id}WorkingCopy")
    val gitWorkingCopy = system.actorOf(GitWorkingCopy(testActor, folder, 50.milliseconds), folder.getName)
    (repo, gitWorkingCopy)
  }

  private def notExistingFolder(folder: String) = {
    val folderFile = new File(folder)
    FileUtils.deleteDirectory(folderFile)
    folderFile
  }
}
