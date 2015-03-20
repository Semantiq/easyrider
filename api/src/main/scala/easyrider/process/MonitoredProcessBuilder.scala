package easyrider.process

import easyrider.Commands.{CommandExecution, Failure, Success}

trait MonitoredProcessBuilder {
  def onSuccess(callback: Success => Unit): MonitoredProcessBuilder
  def onFailure(callback: Failure => Unit): MonitoredProcessBuilder
  def onProgressUpdate(callback: CommandExecution => Unit): MonitoredProcessBuilder
  def onTimeout(callback: () => Unit): MonitoredProcessBuilder
  def run(): CommandMonitor
}
