package easyrider.business.tests

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import easyrider.Api.{AuthenticateUser, Authentication}
import easyrider.Applications._
import easyrider.Events.{Subscribe, Subscribed}
import easyrider.Implicits._
import easyrider.Infrastructure._
import easyrider.Repository.Version
import easyrider.SshInfrastructure.NodeConfiguration
import easyrider._
import easyrider.business.EasyriderTest
import org.apache.commons.io.FileUtils

class SshInfrastructureManagementTest extends EasyriderTest(ActorSystem("test")) {
  "EasyRider" should "allow to add ssh host" in withEasyrider { easyrider =>
    val client = TestProbe()
    val api = easyrider.actorSystem.actorOf(easyrider.core.apiFactory(client.ref))

    client.send(api, AuthenticateUser())
    client.expectMsgClass(classOf[Authentication])

    client.send(api, Subscribe(CommandId.generate(), "nodeEvents", classOf[NodeUpdatedEvent], EventKey()))
    client.expectMsgClass(classOf[Subscribed[_]])

    client.send(api, SshInfrastructure.CreateNode(CommandId.generate(), NodeConfiguration(NodeId("nodeA"), "localhost", 22, "test", "test")))
    client.expectMsgClass(classOf[NodeUpdatedEvent])
  }

  it should "manage containers on ssh host" in {

  }

  it should "deploy & start application packages on a container on ssh host" in withEasyrider { easyrider =>
    val client = TestProbe()
    val api = easyrider.actorSystem.actorOf(easyrider.core.apiFactory(client.ref))
    // TODO: perform upload, to avoid writing to easyrider folders directly
    FileUtils.copyFile(new File("testing/testing.tar.bz2"), new File("target/easyrider/repository/app/1.0.0.tar.bz2"))

    client.send(api, AuthenticateUser())
    client.expectMsgClass(classOf[Authentication])

    client.send(api, Subscribe(CommandId.generate(), "deploymentEvents", classOf[VersionDeploymentProgressEvent], EventKey()))
    client.expectMsgClass(classOf[Subscribed[_]])
    client.send(api, Subscribe(CommandId.generate(), "containerState", classOf[ContainerStateChangedEvent], EventKey()))
    client.expectMsgClass(classOf[Subscribed[_]])

    client.send(api, SshInfrastructure.CreateNode(CommandId.generate(), NodeConfiguration(NodeId("nodeA"), "localhost", 22, "test", "test")))
    client.send(api, Applications.CreateApplication(CommandId.generate(), Application(ApplicationId("app"), Seq())))
    client.send(api, Applications.CreateStage(CommandId.generate(), Stage(StageId(ApplicationId("app"), "qa"), Seq())))
    client.send(api, Applications.CreateContainerConfiguration(CommandId.generate(), ContainerConfiguration(ContainerId(StageId(ApplicationId("app"), "qa"), "0"), NodeId("nodeA"), Seq())))

    client.expectMsgClass(classOf[ContainerStateChangedEvent]).state should be (ContainerCreated)

    client.send(api, Infrastructure.DeployVersion(CommandId.generate(), ContainerId(StageId(ApplicationId("app"), "qa"), "0"), Version(ApplicationId("app"), "1.0.0")))

    client.expectMsgClass(classOf[VersionDeploymentProgressEvent]).state should be (DeploymentInProgress)
    client.expectMsgClass(classOf[VersionDeploymentProgressEvent]).state should be (DeploymentCompleted)

    client.send(api, Infrastructure.StartContainer(CommandId.generate(), ContainerId(StageId(ApplicationId("app"), "qa"), "0"), Version(ApplicationId("app"), "1.0.0")))

    client.expectMsgClass(classOf[ContainerStateChangedEvent]).state should be (ContainerRunning(Version(ApplicationId("app"), "1.0.0")))

    client.send(api, Infrastructure.StopContainer(CommandId.generate(), ContainerId(StageId(ApplicationId("app"), "qa"), "0")))

    client.expectMsgClass(classOf[ContainerStateChangedEvent]).state should be (ContainerStopping(Version(ApplicationId("app"), "1.0.0")))
    client.expectMsgClass(classOf[ContainerStateChangedEvent]).state should be (ContainerCreated)
  }
}
