package easyrider.process

import akka.actor.ActorRef

private[process] class ActorBackedProcessMonitor(actor: ActorRef) extends CommandMonitor {
  override def cancel() = actor ! Cancel
}
