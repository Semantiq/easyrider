package easyrider.business

import java.util.concurrent.TimeUnit

import akka.actor.SupervisorStrategy.Escalate
import akka.actor.{OneForOneStrategy, SupervisorStrategy, SupervisorStrategyConfigurator}

import scala.concurrent.duration.Duration

/**
 * Top Level actors should be un-impregnable. Use Escalate strategy in short term, to hi-light any problems.
 */
class TopLevelSupervisorStrategy extends SupervisorStrategyConfigurator {
  override def create(): SupervisorStrategy = OneForOneStrategy(maxNrOfRetries = 3, withinTimeRange = Duration(1, TimeUnit.MINUTES)) {
    case e: OutOfMemoryError => Escalate
    case other => Escalate
  }
}
