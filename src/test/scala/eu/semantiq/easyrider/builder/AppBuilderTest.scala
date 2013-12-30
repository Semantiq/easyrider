package eu.semantiq.easyrider.builder

import akka.testkit.{TestProbe, TestKit, ImplicitSender}
import akka.actor.ActorSystem
import org.scalatest.FunSpecLike
import java.io.File
import eu.semantiq.easyrider.{Compilation, AppRepository}
import org.scalatest.Matchers._

class AppBuilderTest extends TestKit(ActorSystem("AppBuilderTest")) with ImplicitSender with FunSpecLike {

  it("should build and deploy new version of the application each time a commit is successfully built") {
    val (builder, git, appRepo) = setup("successCase")

    builder ! AppBuilder.ConfigurationUpdated(git.gitURL)
    git.updateFile("compile.sh", "#!/bin/sh\necho Hello > run.sh")
    val deployment = appRepo.expectMsgClass(classOf[AppRepository.DeployVersion])
    deployment.app should be ("successCase")
  }

  private def setup(id: String) = {
    val git = new DummyGitRepository("AppBuilderTest." + id)
    val appRepo = TestProbe()
    val builder = system.actorOf(AppBuilder(id, appRepo.ref, workingDirectory(id)))
    (builder, git, appRepo)
  }

  private def workingDirectory(id: String) = new File(s"target/AppBuilderTest.$id")
}
