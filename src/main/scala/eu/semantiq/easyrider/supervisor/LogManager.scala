package eu.semantiq.easyrider.supervisor

import java.io.File
import scala.concurrent.duration._
import akka.actor.{ActorLogging, Actor, Props}

class LogManager(logsDirectory: File) extends Actor with ActorLogging {
  import LogManager._
  private implicit val executionContext = context.system.dispatcher
  context.system.scheduler.schedule(2.minutes, 2.hours, self, DoMaintenance)

  override def receive: Receive = {
    case DoMaintenance =>
      val now = System.currentTimeMillis()
      for (file <- logsDirectory.listFiles) {
        if (file.isFile && now - file.lastModified > timeToLive) {
          log.info("Deleting old log file: {}", file)
          try {
            file.delete()
          } catch {
            case e: Exception => log.error(e, "Couldn't delete old log file: {}", file)
          }
        }
      }
  }

  private val timeToLive = 5.days.toMillis
}

object LogManager {
  def apply(logsDirectory: File) = Props(classOf[LogManager], logsDirectory)

  private object DoMaintenance
}