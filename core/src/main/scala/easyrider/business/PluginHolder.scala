package easyrider.business

import akka.actor.{ActorRef, Props}
import easyrider.PluginFactory
import easyrider.business.http.HttpWorkersRegistry
import easyrider.business.util.BinaryDataSerializers
import org.json4s.FullTypeHints
import org.json4s.ext.JodaTimeSerializers
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write

class PluginHolder(eventBus: ActorRef, applicationManager: ActorRef, pluginFactory: PluginFactory, pluginName: String,
                    containerPluginManager: ActorRef, nodeManager: ActorRef,
                    httpWorkersRegistry: ActorRef, repository: ActorRef)
  extends Connector(eventBus, applicationManager, containerPluginManager, nodeManager, repository, pluginName) {

  val client = context.actorOf(pluginFactory.props, "plugin")
  val http = pluginFactory.httpHandler(client).map(handler => context.actorOf(handler, "http")).foreach { handler =>
    httpWorkersRegistry ! HttpWorkersRegistry.Register(pluginName, handler)
  }

  private implicit val formats = Serialization.formats(FullTypeHints(List(classOf[AnyRef]))) ++ JodaTimeSerializers.all ++ BinaryDataSerializers.short

  override def logToPlugin(message: AnyRef) = log.info("{} << {}", pluginName, write(message))
  override def logFromPlugin(message: AnyRef) = log.info("{} >> {}", pluginName, write(message))
}

object PluginHolder {
  def apply(eventBus: ActorRef, applicationManager: ActorRef, containerPluginManager: ActorRef, nodeManager: ActorRef,
            httpWorkersRegistry: ActorRef, repository: ActorRef)
           (pluginFactory: PluginFactory, pluginName: String) = Props(classOf[PluginHolder], eventBus, applicationManager,
    pluginFactory, pluginName, containerPluginManager, nodeManager, httpWorkersRegistry, repository)
}
