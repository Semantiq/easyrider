package easyrider.builtin

import akka.actor._
import easyrider.Command
import easyrider.Commands.{CommandExecution, Failure, Success}

import scala.concurrent.duration.{Duration, FiniteDuration}

private[builtin] class ActorBackedProcessMonitor(actor: ActorRef) extends CommandMonitor {
  override def cancel() = actor ! Cancel
}

private[builtin] case object Cancel

private[builtin] class ProcessMonitorActor(settings: ProcessMonitorSettings) extends Actor {
  settings.timeout match {
    case timeout: FiniteDuration => context.setReceiveTimeout(timeout)
    case _ =>
  }

  override def receive: Receive = {
    case success: Success =>
      handleSuccess(success)
    case failure: Failure =>
      handleFailure(failure)
    case progress: CommandExecution =>
      handleProgress(progress)
    case ReceiveTimeout =>
      handleTimeout()
  }

  def handleTimeout() = {
    settings.onTimeoutCallbacks.foreach(callback => callback())
    context.stop(self)
  }

  def handleFailure(failure: Failure): Unit = {
    settings.onFailureCallbacks.foreach(callback => callback(failure))
    context.stop(self)
  }

  def handleSuccess(success: Success): Unit = {
    settings.onSuccessCallbacks.foreach(callback => callback(success))
    context.stop(self)
  }

  def handleProgress(progress: CommandExecution): Unit = {
    settings.onProgressUpdateCallbacks.foreach(callback => callback(progress))
  }
}

object SequentialProcess {
  def apply(actorSystem: ActorRefFactory)(commands: MonitoredProcessBuilder*): MonitoredProcessBuilder = new SequentialProcessBuilder(actorSystem, commands)

  private class SequentialProcessBuilder(actorSystem: ActorRefFactory, steps: Seq[MonitoredProcessBuilder]) extends SettingsBuilder {
    override def run(): CommandMonitor = {
      val actor = actorSystem.actorOf(Props(new SequentialProcessMonitorActor(steps.toList, settings)))
      new ActorBackedProcessMonitor(actor)
    }
  }

  private class SequentialProcessMonitorActor(commands: List[MonitoredProcessBuilder], settings: ProcessMonitorSettings)
      extends ProcessMonitorActor(settings) with ActorLogging {
    var remainingSteps = commands.tail
    startStep(commands.head)

    override def handleSuccess(success: Success) = remainingSteps match {
      case Nil => super.handleSuccess(success)
      case currentStep :: nextSteps =>
        startStep(currentStep)
        remainingSteps = nextSteps
    }

    private def startStep(step: MonitoredProcessBuilder) = {
      log.debug("Starting step: {}", step)
      step.onFailure(forwardToSelf).onSuccess(forwardToSelf).onProgressUpdate(forwardToSelf)
        .onTimeout(() => self ! ReceiveTimeout)
        .run()
    }

    private def forwardToSelf[T](message: T) = self ! message
  }
}

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

trait CommandMonitor {
  def cancel(): Unit
}

trait MonitoredProcessBuilder {
  def onSuccess(callback: Success => Unit): MonitoredProcessBuilder
  def onFailure(callback: Failure => Unit): MonitoredProcessBuilder
  def onProgressUpdate(callback: CommandExecution => Unit): MonitoredProcessBuilder
  def onTimeout(callback: () => Unit): MonitoredProcessBuilder
  def run(): CommandMonitor
}

case class ProcessMonitorSettings(onSuccessCallbacks: Seq[(Success) => Unit] = Seq(),
                                  onProgressUpdateCallbacks: Seq[(CommandExecution) => Unit] = Seq(),
                                  onTimeoutCallbacks: Seq[() => Unit] = Seq(),
                                  onFailureCallbacks: Seq[(Failure) => Unit] = Seq(),
                                  timeout: Duration = Duration.Inf)

trait SettingsBuilder extends MonitoredProcessBuilder {
  protected var settings = ProcessMonitorSettings()

  override def onSuccess(callback: (Success) => Unit): MonitoredProcessBuilder = {
    settings = settings.copy(onSuccessCallbacks = settings.onSuccessCallbacks :+ callback)
    this
  }

  override def onProgressUpdate(callback: (CommandExecution) => Unit): MonitoredProcessBuilder = {
    settings = settings.copy(onProgressUpdateCallbacks = settings.onProgressUpdateCallbacks :+ callback)
    this
  }

  override def onTimeout(callback: () => Unit): MonitoredProcessBuilder = {
    settings = settings.copy(onTimeoutCallbacks = settings.onTimeoutCallbacks :+ callback)
    this
  }

  override def onFailure(callback: (Failure) => Unit): MonitoredProcessBuilder = {
    settings = settings.copy(onFailureCallbacks = settings.onFailureCallbacks :+ callback)
    this
  }
}

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

