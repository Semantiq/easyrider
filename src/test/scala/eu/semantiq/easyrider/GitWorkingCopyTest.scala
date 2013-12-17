package eu.semantiq.easyrider

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.{Props, ActorSystem}
import org.scalatest.{BeforeAndAfter, Matchers, FunSpecLike}
import org.apache.commons.io.FileUtils
import java.io.File
import scala.concurrent.duration._

class GitWorkingCopyTest extends TestKit(ActorSystem("GitWorkingCopyTest")) with ImplicitSender with FunSpecLike with Matchers {
  it("should clone the repository at startup") {
    val id = "cloneTest"
    val repo = new DummyGitRepository(id)
    val folder = notExistingFolder(s"target/${id}WorkingCopy")
    val gitWorkingCopy = makeGitWorkingCopy(folder)

    gitWorkingCopy ! GitWorkingCopy.ConfigurationUpdated(repo.gitURL)
    expectMsg(AppSupervisor.WorkingCopyUpdated)
  }

  it("should poll for incoming changes") {
    val id = "pollingTest"
    val repo = new DummyGitRepository(id)
    val folder = notExistingFolder(s"target/${id}WorkingCopy")
    val gitWorkingCopy = makeGitWorkingCopy(folder)

    gitWorkingCopy ! GitWorkingCopy.ConfigurationUpdated(repo.gitURL)
    expectMsg(AppSupervisor.WorkingCopyUpdated)
    expectNoMsg()
    repo.updateFile("anything", "new content")
    expectMsg(15.seconds, AppSupervisor.WorkingCopyUpdated)
    expectNoMsg()
  }

  private def makeGitWorkingCopy(folder: File) = {
    system.actorOf(GitWorkingCopy(testActor, folder), folder.getName)
  }

  private def notExistingFolder(folder: String) = {
    val folderFile = new File(folder)
    FileUtils.deleteDirectory(folderFile)
    folderFile
  }
}
