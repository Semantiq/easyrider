package easyrider.business

import akka.actor.{Actor, ActorLogging, ActorRef}
import akka.event.LoggingReceive
import easyrider.Commands.{CommandExecution, Failure, Success}
import easyrider.Events.{EventBusCommand, GetSnapshot, GetSnapshotResponse, Subscribed}
import easyrider.Infrastructure.ContainerCommand
import easyrider.Plugins.{NotifyNodeStatus, RegisterContainerPlugin, RegisterNodeManagementPlugin}
import easyrider.RemoteAccess.RemoteAccessCommand
import easyrider.Repository.RepositoryCommand
import easyrider.json.JsonSerializer
import easyrider.{Command, CommandId, Event}

abstract class Connector(eventBus: ActorRef, applicationManager: ActorRef,
                         containerPluginManager: ActorRef, nodeManager: ActorRef,
                         repository: ActorRef,
                         clientName: String) extends Actor with ActorLogging {

  val client: ActorRef

  private var commands = Map[CommandId, ActorRef]()
  private val serializer = new JsonSerializer()

  override def receive = LoggingReceive {
    case m: AnyRef if sender == client =>
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
    case m: AnyRef if sender != client =>
      logToPlugin(m)
      m match {
        case e: Event => client ! e
        case r: GetSnapshotResponse[_] => client ! r
        case c: Command =>
          client ! c
          commands += c.commandDetails.commandId -> sender
        case s: Subscribed[_] => client ! s
      }
  }

  def logToPlugin(message: AnyRef) = log.info("{} << {}", clientName, serializer.write(message.asInstanceOf[Event]))
  def logFromPlugin(message: AnyRef) = log.info("{} >> {}", clientName, serializer.write(message.asInstanceOf[Command]))
}
