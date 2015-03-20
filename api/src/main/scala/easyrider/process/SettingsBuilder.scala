package easyrider.process

import easyrider.Commands._

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
