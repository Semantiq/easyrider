package easyrider.process

import akka.actor.{Props, ReceiveTimeout, ActorRefFactory}
import easyrider.Commands.Success

object ParallelProcess {
  def apply(actorRefFactory: ActorRefFactory)(successMapper: Seq[Success] => Success, commands: MonitoredProcessBuilder*): MonitoredProcessBuilder = new ParallelProcessBuilder(actorRefFactory, successMapper, commands)

  private class ParallelProcessBuilder(actorRefFactory: ActorRefFactory, successMapper: Seq[Success] => Success, commands: Seq[MonitoredProcessBuilder]) extends SettingsBuilder {
    override def run(): CommandMonitor = {
      val actor = actorRefFactory.actorOf(Props(new ParallelProcessMonitorActor(successMapper, commands, settings)))
      new ActorBackedProcessMonitor(actor)
    }
  }

  private class ParallelProcessMonitorActor(successMapper: Seq[Success] => Success, commands: Seq[MonitoredProcessBuilder], settings: ProcessMonitorSettings) extends ProcessMonitorActor(settings) {
    commands.foreach { command =>
      command.onFailure(forwardToSelf).onProgressUpdate(forwardToSelf)
        .onSuccess(message => self ! SubProcessCompeted(command, message))
        .onTimeout(() => self ! ReceiveTimeout)
        .run()
    }

    var pendingCommands = commands.toSet
    var successes = Seq[Success]()

    override def receive = parallelProcessUpdates orElse super.receive

    private def parallelProcessUpdates: Receive = {
      case SubProcessCompeted(command, message) =>
        pendingCommands -= command
        successes +:= message
        if (pendingCommands.isEmpty) {
          val success = successMapper(successes)
          settings.onSuccessCallbacks.foreach(callback => callback(success))
          context.stop(self)
        }
    }

    private def forwardToSelf[T](message: T) = self ! message
    private case class SubProcessCompeted(command: MonitoredProcessBuilder, message: Success)
  }
}
