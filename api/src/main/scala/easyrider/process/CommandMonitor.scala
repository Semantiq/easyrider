package easyrider.process

import akka.actor.{Props, ActorLogging, ActorRef, ActorRefFactory}
import easyrider.Command

object CommandMonitor {
  def apply(actorSystem: ActorRefFactory)(runner: ActorRef, command: Command): MonitoredProcessBuilder = new CommandMonitorBuilder(actorSystem, runner, command)

  private class CommandMonitorBuilder(actorSystem: ActorRefFactory, runner: ActorRef, command: Command) extends SettingsBuilder {
    override def run(): CommandMonitor = {
      val actor = actorSystem.actorOf(CommandMonitorActor(runner, command, settings), command.commandDetails.commandId.id)
      new ActorBackedProcessMonitor(actor)
    }

    override def toString = s"CommandMonitorBuilder($runner, $command)"
  }

  private class CommandMonitorActor(runner: ActorRef, command: Command, settings: ProcessMonitorSettings)
      extends ProcessMonitorActor(settings) with ActorLogging {
    log.debug("Sending command {} to {}", command, runner)
    runner ! command
  }

  private object CommandMonitorActor {
    def apply(runner: ActorRef, command: Command, settings: ProcessMonitorSettings) = Props(new CommandMonitorActor(runner, command, settings))
  }

}

trait CommandMonitor {
  def cancel(): Unit
}