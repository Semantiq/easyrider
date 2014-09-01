package easyrider.business

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.apache.commons.io.FileUtils
import org.scalatest.{FlatSpecLike, Matchers}

abstract class EasyriderTest(actorSystem: ActorSystem) extends TestKit(actorSystem) with FlatSpecLike with Matchers with ImplicitSender {
  def withEasyrider(test: Easyrider => Unit): Unit = {
    val data = new File("target/easyrider")
    FileUtils.deleteDirectory(data)
    val easyrider = new Easyrider(8081, data)
    test(easyrider)
    easyrider.actorSystem.shutdown()
    easyrider.actorSystem.awaitTermination()
  }
}
