package easyrider.business

import java.io.File
import java.net.URL
import java.util.ServiceLoader

import akka.actor.ActorSystem
import easyrider.business.core.CoreModule
import easyrider.business.http.HttpModule
import easyrider.{PageProvider, PluginFactory}

import scala.collection.JavaConversions._

class EasyRider(port: Int, easyRiderData: File) {
  val easyRiderUrl = new URL(s"http://localhost:$port")
  val actorSystem = ActorSystem("EasyRider")
  val core = new CoreModule(easyRiderData, easyRiderUrl, actorSystem)
  val pages = ServiceLoader.load(classOf[PageProvider]).iterator().toSeq
  val http = new HttpModule(actorSystem, core.apiFactory, port, pages)
  val plugins = ServiceLoader.load(classOf[PluginFactory]).iterator().toSeq
  println("pages: " + pages)
  println("plugins: " + plugins)
  val pluginHolderFactory = PluginHolder(core.eventBus, core.applicationManager, core.containerPluginManager, core.nodeManager,
    http.workersRegistry, core.repository) _

  plugins.foreach { p =>
    actorSystem.actorOf(pluginHolderFactory(p, p.getClass.getSimpleName), p.getClass.getSimpleName)
  }
}
