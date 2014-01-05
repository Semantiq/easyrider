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

    val message = receiveOne(3.seconds).asInstanceOf[Reconfigured]
    message.configuration should have size 1
  }

  it("should send an update if configuration is changed") {
    val configFile = new DummyConfigFile("sendConfigUpdate")
    system.actorOf(ConfigurationManager(testActor, configFile.location, checkInterval = 100.microseconds), "updating-configuration-manager")

    receiveOne(200.milliseconds)
    expectNoMsg(200.milliseconds)
    configFile.update("[]")
    receiveOne(200.milliseconds)
  }
}
