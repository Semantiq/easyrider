package easyrider.process

import akka.actor.{ReceiveTimeout, Actor}
import easyrider.Commands.{CommandExecution, Failure, Success}

import scala.concurrent.duration.FiniteDuration

private[process] class ProcessMonitorActor(settings: ProcessMonitorSettings) extends Actor {
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
