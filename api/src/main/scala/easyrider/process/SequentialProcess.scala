package easyrider.process

import akka.actor.{ReceiveTimeout, ActorLogging, Props, ActorRefFactory}
import easyrider.Commands.Success

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
