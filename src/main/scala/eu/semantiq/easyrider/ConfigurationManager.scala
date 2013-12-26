package eu.semantiq.easyrider

import java.io.File
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import java.nio.file.FileSystems

class ConfigurationManager(listener: ActorRef, source: File, checkInterval: FiniteDuration) extends Actor with ActorLogging {
	import ConfigurationManager._
	private implicit val formats = DefaultFormats

	private var configuration = getConfiguration()

	private val timerSubscription = context.system.scheduler.schedule(checkInterval, checkInterval, self, CheckForConfigurationChanges)
  //private val watch = context.actorOf(Props[FileSystemWatchActor], "watch")
  //watch ! FileSystemWatchActor.Subscribe(source.getAbsoluteFile, self)

  listener ! Reconfigured(configuration)

	override def postStop() {
		timerSubscription.cancel()
	}

	def receive: Receive = {
		//case FileSystemWatchActor.FileModified(file) => {
    case CheckForConfigurationChanges =>
			val checked = getConfiguration
      if (configuration != checked) {
				configuration = checked
        log.info("Found new configuration: {}", configuration)
				listener ! Reconfigured(configuration)
			}
	}

	private def getConfiguration() = getConfigurationFor(source)
	private def getConfigurationForFile(f: File) = {
		val json = parse(f)
		if (json.isInstanceOf[JArray])
			json.extract[Seq[Application]]
		else
			Seq(json.extract[Application])
	}
	private def getConfigurationFor(source: File): Seq[Application] = {
		if (source.isFile)
			getConfigurationForFile(source)
		else
			source.listFiles.filter(_.getName.toLowerCase.endsWith(".json")).flatMap(getConfigurationFor)
	}
}

object ConfigurationManager {
	def apply(configurationListener: ActorRef, source: File, checkInterval: FiniteDuration = 30.seconds) = Props(classOf[ConfigurationManager],
		configurationListener, source, checkInterval)
	case class Reconfigured(configuration: Seq[Application])
	private object CheckForConfigurationChanges
}
