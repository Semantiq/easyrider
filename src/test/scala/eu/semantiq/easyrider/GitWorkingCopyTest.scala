package eu.semantiq.easyrider

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.{Props, ActorSystem}
import org.scalatest.{BeforeAndAfter, Matchers, FunSpecLike}
import org.apache.commons.io.FileUtils
import java.io.File
import scala.concurrent.duration._

class GitWorkingCopyTest extends TestKit(ActorSystem("GitWorkingCopyTest")) with ImplicitSender with FunSpecLike with Matchers {
  it("should clone the repository at startup") {
    // given
    val repo = new DummyGitRepository("cloneTest")
    val folder = notExistingFolder("target/cloneTestWorkingCopy")
    // when
    val gitWorkingCopy = makeGitWorkingCopy(folder, repo)
    // then
    gitWorkingCopy ! GitWorkingCopy.Activate
    expectMsg(AppSupervisor.WorkingCopyUpdated)
  }

  private def makeGitWorkingCopy(folder: File, repo: DummyGitRepository) = {
    system.actorOf(Props(classOf[GitWorkingCopy], testActor, folder.getName, repo.gitURL, folder.getParentFile), folder.getName)
  }

  it("should poll for incoming changes") {
    // given
    val repo = new DummyGitRepository("pollingTest")
    val folder = notExistingFolder("target/pollingTestWorkingCopy")
    // when
    val gitWorkingCopy = makeGitWorkingCopy(folder, repo)
    // then
    gitWorkingCopy ! GitWorkingCopy.Activate
    expectMsg(AppSupervisor.WorkingCopyUpdated)
    expectNoMsg
    repo.updateFile("anything", "new content")
    expectMsg(15.seconds, AppSupervisor.WorkingCopyUpdated)
    expectNoMsg
  }

  private def notExistingFolder(folder: String) = {
    val folderFile = new File(folder)
    FileUtils.deleteDirectory(folderFile)
    folderFile
  }
}
