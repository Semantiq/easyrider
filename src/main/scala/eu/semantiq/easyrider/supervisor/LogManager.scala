package eu.semantiq.easyrider.supervisor

import java.io.File
import scala.concurrent.duration._
import akka.actor.{ActorLogging, Actor, Props}
import org.apache.commons.io.input.{TailerListenerAdapter, Tailer}

class LogManager(app: String, logsDirectory: File) extends Actor with ActorLogging {
  import LogManager._
  private implicit val executionContext = context.system.dispatcher
  context.system.scheduler.schedule(2.minutes, 2.hours, self, DoMaintenance)

  private val logFile = new File(logsDirectory, s"$app.log")
  val tailer = new Tailer(logFile, new TailerListenerAdapter {
    override def handle(line: String) = self ! Line(line)
  }, 200, false, true)

  override def preStart() {
    log.info(s"Watching log file: $logFile")
    executionContext.execute(tailer)
  }

  override def postStop() {
    tailer.stop()
  }

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
    case Line(line) =>
      if (line.contains("ERROR")) {
        log.error(s"Error in $app: $line")
      }
  }

  private val timeToLive = 5.days.toMillis
}

object LogManager {
  def apply(app: String, logsDirectory: File) = Props(classOf[LogManager], app, logsDirectory)

  private object DoMaintenance
  private case class Line(line: String)
}