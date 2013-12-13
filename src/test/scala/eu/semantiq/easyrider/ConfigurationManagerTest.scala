package eu.semantiq.easyrider

import akka.actor.{Props, ActorSystem}
import akka.testkit.{TestProbe, ImplicitSender, TestKit}
import org.scalatest.{Matchers, FunSpecLike}
import scala.concurrent.duration._
import eu.semantiq.easyrider.ConfigurationManager.Reconfigured
import java.io.File

class ConfigurationManagerTest extends TestKit(ActorSystem("ConfigurationManagerTest")) with ImplicitSender with FunSpecLike with Matchers {
  it("should send the current configuration at startup") {
    // given
    val probe = TestProbe()
    // when
    system.actorOf(ConfigurationManager(probe.ref, new File("src/test/resources/test-configuration.json")), "configuration-manager")
    // then
    val message = probe.receiveOne(1.second).asInstanceOf[Reconfigured]
    message.configuration should have size 1
  }

  it("should send an update if configuration is changed") {
    // given
    val probe = TestProbe()
    val configFile = new DummyConfigFile
    // when
    system.actorOf(ConfigurationManager(probe.ref, configFile.location, checkInterval = 1.second), "updating-configuration-manager")
    // then
    probe.receiveOne(1.second)
    probe.expectNoMsg(5.seconds)
    configFile.update("[]")
    probe.receiveOne(5.seconds)
  }
}
