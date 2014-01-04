package eu.semantiq.easyrider.builder

import akka.testkit.{TestProbe, TestKit, ImplicitSender}
import akka.actor.ActorSystem
import org.scalatest.FunSpecLike
import java.io.File
import eu.semantiq.easyrider.AppRepository
import org.scalatest.Matchers._
import eu.semantiq.easyrider.AppRepository.PackageRef
import org.scalatest.matchers.{MatchResult, Matcher}
import org.apache.commons.io.FileUtils
import scala.util.Random
import scala.concurrent.duration._

class AppBuilderTest extends TestKit(ActorSystem("AppBuilderTest")) with ImplicitSender with FunSpecLike {

  it("should build and deploy new version of the application each time a commit is successfully built") {
    val (builder, git, appRepo, temp) = setup("successCase")

    builder ! AppBuilder.ConfigurationUpdated(git.gitURL)
    git.updateFile("compile.sh", "#!/bin/sh\necho Hello > run.sh")
    val deployment = appRepo.expectMsgClass(classOf[AppRepository.DeployVersion])
    deployment.appPackage.extractTo(temp)

    deployment.app should be ("successCase")
    new File(temp, "run.sh") should exist
  }

  it("should respond to git repository url change") {
    val (builder, git, appRepo, _) = setup("gitUrlChange")

    val otherGit = new DummyGitRepository("AppBuilderTest.gitUrlChange.other")
    git.updateFile("marker", "old")
    otherGit.updateFile("marker", "new")

    builder ! AppBuilder.ConfigurationUpdated(git.gitURL)
    appRepo.expectMsgClass(classOf[AppRepository.DeployVersion]).appPackage should containFile("marker", that = include("old"))
    builder ! AppBuilder.ConfigurationUpdated(otherGit.gitURL)
    appRepo.expectMsgClass(classOf[AppRepository.DeployVersion]).appPackage should containFile("marker", that = include("new"))
  }

  private def setup(id: String) = {
    val git = new DummyGitRepository("AppBuilderTest." + id)
    val appRepo = TestProbe()
    val builder = system.actorOf(AppBuilder(id, appRepo.ref, workingDirectory(id), gitPollingInterval = 500.millis), id)
    val temp = tempDirectory(id)
    temp.mkdir()
    (builder, git, appRepo, temp)
  }

  private def containFile(name: String, that: Matcher[String]) = new Matcher[PackageRef] {
    def apply(left: PackageRef): MatchResult = {
      val location = new File(s"target/AppBuilderTest.${Random.nextLong()}")
      left.extractTo(location)
      val file = new File(location, name)
      if (file.exists()) {
        that
          .mapResult(r => MatchResult(r.matches, s"package file '$file' ${r.failureMessage}", s"package file '$file' ${r.negatedFailureMessage}"))
          .apply(FileUtils.readFileToString(file))
      } else {
        MatchResult(matches = false, s"package didn't contain file '$file'", s"package containing file '$file'")
      }
    }
  }

  private def workingDirectory(id: String) = nonExistentDirectory(new File(s"target/AppBuilderTest.$id.working"))
  private def tempDirectory(id: String) = nonExistentDirectory(new File(s"target/AppBuilderTest.$id.temp"))
  private def nonExistentDirectory(directory: File) = {
    FileUtils.deleteDirectory(directory)
    directory
  }
}
