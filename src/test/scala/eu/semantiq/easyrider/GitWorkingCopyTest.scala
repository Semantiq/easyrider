package eu.semantiq.easyrider

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.{Props, ActorSystem}
import org.scalatest.{BeforeAndAfter, Matchers, FunSpecLike}
import org.apache.commons.io.FileUtils
import java.io.File
import scala.concurrent.duration._

class GitWorkingCopyTest extends TestKit(ActorSystem("GitWorkingCopyTest")) with ImplicitSender with FunSpecLike with Matchers {
  it("should clone the repository at startup") {
    val (repo, gitWorkingCopy) = setup("cloneTest")

    gitWorkingCopy ! GitWorkingCopy.ConfigurationUpdated(repo.gitURL)
    expectMsg(AppSupervisor.WorkingCopyUpdated)
  }

  it("should poll for incoming changes") {
    val (repo, gitWorkingCopy) = setup("pollingTest")

    gitWorkingCopy ! GitWorkingCopy.ConfigurationUpdated(repo.gitURL)
    expectMsg(AppSupervisor.WorkingCopyUpdated)
    expectNoMsg(1.second)
    repo.updateFile("anything", "new content")
    expectMsg(AppSupervisor.WorkingCopyUpdated)
    expectNoMsg(1.second)
  }

  private def setup(id: String) = {
    val repo = new DummyGitRepository(id)
    val folder = notExistingFolder(s"target/${id}WorkingCopy")
    val gitWorkingCopy = system.actorOf(GitWorkingCopy(testActor, folder, 200.milliseconds), folder.getName)
    (repo, gitWorkingCopy)
  }

  private def notExistingFolder(folder: String) = {
    val folderFile = new File(folder)
    FileUtils.deleteDirectory(folderFile)
    folderFile
  }
}
