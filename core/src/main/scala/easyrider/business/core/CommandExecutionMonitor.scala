package easyrider.business.core

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import easyrider.Api.CommandSentEvent
import easyrider.Commands.{Failure, Success}
import easyrider.Events.{StartSnapshotSubscription, Subscribe}
import easyrider.Implicits._
import easyrider._

import scala.concurrent.duration._
import scala.language.postfixOps

class CommandExecutionMonitor(eventBus: ActorRef) extends Actor with ActorLogging {
  private case class CommandTimedOut(commandId: CommandId)

  implicit val dispatcher = context.system.dispatcher
  var runningCommands = Map[CommandId, Command]()

  eventBus ! Subscribe(CommandDetails(), "commands", classOf[CommandSentEvent], EventKey())
  eventBus ! Subscribe(CommandDetails(), "successes", classOf[Success], EventKey())
  eventBus ! Subscribe(CommandDetails(), "failures", classOf[Failure], EventKey())

  override def receive: Receive = {
    case CommandSentEvent(_, command, _, _) =>
      if (!command.isInstanceOf[StartSnapshotSubscription[_]]) {
        runningCommands += command.commandDetails.commandId -> command
        context.system.scheduler.scheduleOnce(30 seconds, self, CommandTimedOut(command.commandDetails.commandId))
      }
    case success: Success =>
      runningCommands -= success.executionOf
    case failure: Failure =>
      runningCommands -= failure.executionOf
    case CommandTimedOut(commandId) if runningCommands contains commandId =>
      log.warning("Command did not complete in timely fashion: {}", runningCommands(commandId))
      runningCommands -= commandId
  }
}

object CommandExecutionMonitor {
  def apply(eventBus: ActorRef) = Props(classOf[CommandExecutionMonitor], eventBus)
}
