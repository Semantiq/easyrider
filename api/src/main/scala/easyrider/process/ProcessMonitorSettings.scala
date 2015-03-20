package easyrider.process

import easyrider.Commands.{Failure, CommandExecution, Success}

import scala.concurrent.duration.Duration

case class ProcessMonitorSettings(onSuccessCallbacks: Seq[(Success) => Unit] = Seq(),
                                  onProgressUpdateCallbacks: Seq[(CommandExecution) => Unit] = Seq(),
                                  onTimeoutCallbacks: Seq[() => Unit] = Seq(),
                                  onFailureCallbacks: Seq[(Failure) => Unit] = Seq(),
                                  timeout: Duration = Duration.Inf)
