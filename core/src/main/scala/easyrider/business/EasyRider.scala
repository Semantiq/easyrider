package easyrider.business

import java.io.File
import java.net.URL
import java.util.ServiceLoader

import akka.actor.ActorSystem
import easyrider.{PluginFactory, PageProvider}
import easyrider.business.core.CoreModule
import easyrider.business.http.HttpModule

import scala.collection.JavaConversions._

class EasyRider(port: Int, easyRiderData: File) {
  val easyRiderUrl = new URL(s"http://localhost:$port")
  val actorSystem = ActorSystem("EasyRider")
  val core = new CoreModule(easyRiderData, easyRiderUrl, actorSystem)
  val pages = ServiceLoader.load(classOf[PageProvider]).iterator().toSeq
  val pluginHttpWorkers = Map("builtin" -> core.builtinHttpWorker)
  val http = new HttpModule(actorSystem, core.apiFactory, pluginHttpWorkers, port, pages)
  val plugins = ServiceLoader.load(classOf[PluginFactory]).iterator().toSeq
  println("pages: " + pages)
  println("plugins: " + plugins)
  plugins.foreach { p =>
    actorSystem.actorOf(PluginHolder(core.eventBus, core.applicationManager)(p.props, p.getClass.getSimpleName), p.getClass.getSimpleName)
  }
}
