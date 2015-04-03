package easyrider.business

import akka.actor.{ActorLogging, Actor, ActorRef, Props}
import akka.event.LoggingReceive
import easyrider.Commands.{CommandExecution, Failure, Success}
import easyrider.Plugins.{RegisterNodeManagementPlugin, NotifyNodeStatus, RegisterContainerPlugin}
import easyrider.RemoteAccess.RemoteAccessCommand
import easyrider.Repository.RepositoryCommand
import easyrider.business.http.HttpWorkersRegistry
import easyrider.business.util.BinaryDataSerializers
import easyrider.{PluginFactory, CommandId, Command, Event}
import easyrider.Events.{Subscribed, GetSnapshotResponse, GetSnapshot, EventBusCommand}
import easyrider.Infrastructure.ContainerCommand
import org.json4s.FullTypeHints
import org.json4s.ext.JodaTimeSerializers
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write

class PluginHolder(eventBus: ActorRef, applicationManager: ActorRef, pluginFactory: PluginFactory, pluginName: String,
                    containerPluginManager: ActorRef, nodeManager: ActorRef,
                    httpWorkersRegistry: ActorRef, repository: ActorRef) extends Actor with ActorLogging {
  val plugin = context.actorOf(pluginFactory.props, "plugin")
  val http = pluginFactory.httpHandler(plugin).map(handler => context.actorOf(handler, "http")).foreach { handler =>
    httpWorkersRegistry ! HttpWorkersRegistry.Register(pluginName, handler)
  }

  var commands = Map[CommandId, ActorRef]()

  private implicit val formats = Serialization.formats(FullTypeHints(List(classOf[AnyRef]))) ++ JodaTimeSerializers.all ++ BinaryDataSerializers.short

  override def receive = LoggingReceive {
    case m: AnyRef if sender == plugin =>
      logFromPlugin(m)
      m match {
        case c: EventBusCommand => eventBus ! c
        case c: ContainerCommand => applicationManager ! c
        case c: GetSnapshot => eventBus ! c
        case e: Event =>
          eventBus ! e
          e match {
            case execution: CommandExecution =>
              commands.foreach {
                case (commandId, receiver) if execution.executionOf == commandId =>
                  receiver ! e
                  e match {
                    case e: Success => commands -= commandId
                    case e: Failure => commands -= commandId
                    case _ =>
                  }
                case _ => // ignore
              }
            case _ =>
          }
        case c: RegisterContainerPlugin => containerPluginManager ! c
        case notify: NotifyNodeStatus => nodeManager ! notify
        case c: RemoteAccessCommand => nodeManager ! c
        case c: RegisterNodeManagementPlugin => nodeManager ! c
        case c: RepositoryCommand => repository ! c
      }
    case m: AnyRef if sender != plugin =>
      logToPlugin(m)
      m match {
        case e: Event => plugin ! e
        case r: GetSnapshotResponse[_] => plugin ! r
        case c: Command =>
          plugin ! c
          commands += c.commandDetails.commandId -> sender
        case s: Subscribed[_] => plugin ! s
      }
  }

  def logToPlugin(message: AnyRef) = log.info("{} << {}", pluginName, write(message))
  def logFromPlugin(message: AnyRef) = log.info("{} >> {}", pluginName, write(message))
}

object PluginHolder {
  def apply(eventBus: ActorRef, applicationManager: ActorRef, containerPluginManager: ActorRef, nodeManager: ActorRef,
            httpWorkersRegistry: ActorRef, repository: ActorRef)
           (pluginFactory: PluginFactory, pluginName: String) = Props(classOf[PluginHolder], eventBus, applicationManager,
    pluginFactory, pluginName, containerPluginManager, nodeManager, httpWorkersRegistry, repository)
}
