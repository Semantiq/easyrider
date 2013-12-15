package eu.semantiq.easyrider

import akka.actor.{Props, ActorSystem}
import akka.testkit.{TestProbe, ImplicitSender, TestKit}
import org.scalatest.{Matchers, FunSpecLike}
import scala.concurrent.duration._
import eu.semantiq.easyrider.ConfigurationManager.Reconfigured
import java.io.File

class ConfigurationManagerTest extends TestKit(ActorSystem("ConfigurationManagerTest")) with ImplicitSender with FunSpecLike with Matchers {
  it("should send the current configuration at startup") {
    system.actorOf(ConfigurationManager(testActor, new File("src/test/resources/test-configuration.json")), "configuration-manager")

    val message = receiveOne(1.second).asInstanceOf[Reconfigured]
    message.configuration should have size 1
  }

  it("should send an update if configuration is changed") {
    val configFile = new DummyConfigFile
    system.actorOf(ConfigurationManager(testActor, configFile.location, checkInterval = 1.second), "updating-configuration-manager")

    receiveOne(1.second)
    expectNoMsg(5.seconds)
    configFile.update("[]")
    receiveOne(5.seconds)
  }
}
