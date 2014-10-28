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
import easyrider.business.ssh.SshInfrastructure
import SshInfrastructure.NodeConfiguration
import easyrider._
import easyrider.business.EasyRiderTest
import org.apache.commons.io.FileUtils

class SshInfrastructureManagementTest extends EasyRiderTest(ActorSystem("test")) {
  "EasyRider" should "allow to add ssh host" in withEasyrider { easyrider =>
    val client = TestProbe()
    val api = easyrider.actorSystem.actorOf(easyrider.core.apiFactory(client.ref))

    client.send(api, AuthenticateUser())
    client.expectMsgClass(classOf[Authentication])

    client.send(api, Subscribe(CommandDetails(CommandId.generate(), TraceMode()), "nodeEvents", classOf[NodeUpdatedEvent], EventKey()))
    client.expectMsgClass(classOf[Subscribed[_]])

    client.send(api, SshInfrastructure.CreateNode(CommandDetails(CommandId.generate(), TraceMode()), NodeConfiguration(NodeId("nodeA"), "localhost", 22, "test", "test")))
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

    client.send(api, Subscribe(CommandDetails(CommandId.generate(), TraceMode()), "deploymentEvents", classOf[VersionDeploymentProgressEvent], EventKey()))
    client.expectMsgClass(classOf[Subscribed[_]])
    client.send(api, Subscribe(CommandDetails(CommandId.generate(), TraceMode()), "containerState", classOf[ContainerStateChangedEvent], EventKey()))
    client.expectMsgClass(classOf[Subscribed[_]])

    client.send(api, SshInfrastructure.CreateNode(CommandDetails(CommandId.generate(), TraceMode()), NodeConfiguration(NodeId("nodeA"), "localhost", 22, "test", "test")))
    client.send(api, Applications.CreateApplication(CommandDetails(CommandId.generate(), TraceMode()), Application(ApplicationId("app"), Seq())))
    client.send(api, Applications.CreateStage(CommandDetails(CommandId.generate(), TraceMode()), Stage(StageId(ApplicationId("app"), "qa"), Seq())))
    client.send(api, Applications.CreateContainerConfiguration(CommandDetails(CommandId.generate(), TraceMode()), ContainerConfiguration(ContainerId(StageId(ApplicationId("app"), "qa"), "0"), NodeId("nodeA"), Seq())))

    client.expectMsgClass(classOf[ContainerStateChangedEvent]).state should be (ContainerCreated)

    client.send(api, Infrastructure.DeployVersion(CommandDetails(CommandId.generate(), TraceMode()), ContainerId(StageId(ApplicationId("app"), "qa"), "0"), Version(ApplicationId("app"), "1.0.0")))

    client.expectMsgClass(classOf[VersionDeploymentProgressEvent]).state should be (DeploymentInProgress)
    client.expectMsgClass(classOf[VersionDeploymentProgressEvent]).state should be (DeploymentCompleted)

    client.send(api, Infrastructure.StartContainer(CommandDetails(CommandId.generate(), TraceMode()), ContainerId(StageId(ApplicationId("app"), "qa"), "0"), Version(ApplicationId("app"), "1.0.0")))

    client.expectMsgClass(classOf[ContainerStateChangedEvent]).state should be (ContainerRunning(Version(ApplicationId("app"), "1.0.0")))

    client.send(api, Infrastructure.StopContainer(CommandDetails(CommandId.generate(), TraceMode()), ContainerId(StageId(ApplicationId("app"), "qa"), "0")))

    client.expectMsgClass(classOf[ContainerStateChangedEvent]).state should be (ContainerStopping(Version(ApplicationId("app"), "1.0.0")))
    client.expectMsgClass(classOf[ContainerStateChangedEvent]).state should be (ContainerCreated)
  }
}
