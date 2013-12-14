package eu.semantiq.easyrider

import java.io.File
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor._
import org.json4s._
import org.json4s.jackson.JsonMethods._

class ConfigurationManager(listener: ActorRef, source: File, checkInterval: FiniteDuration) extends Actor {
	import ConfigurationManager._
	private implicit val formats = DefaultFormats

	private var configuration = getConfiguration()

	private val timerSubscription = context.system.scheduler.schedule(checkInterval, checkInterval, self, CheckForConfigurationChanges)
	listener ! Reconfigured(configuration)

	override def postStop() {
		timerSubscription.cancel()
	}

	def receive: Receive = {
		case CheckForConfigurationChanges => {
			val checked = getConfiguration()
			if (getConfiguration != checked) {
				configuration = checked
				listener ! Reconfigured(configuration)
			}
		}
	}

	private def getConfiguration() = getConfigurationFor(source)
	private def getConfigutationForFile(f: File) = {
		val json = parse(f)
		if (json.isInstanceOf[JArray])
			json.extract[Seq[Application]]
		else
			Seq(json.extract[Application])
	}
	private def getConfigurationFor(source: File): Seq[Application] = {
		if (source.isFile)
			getConfigutationForFile(source)
		else
			source.listFiles.filter(_.getName.toLowerCase.endsWith(".json")).flatMap(getConfigurationFor(_))
	}
}

object ConfigurationManager {
	def apply(configurationListener: ActorRef, source: File, checkInterval: FiniteDuration = 30.seconds) = Props(classOf[ConfigurationManager],
		configurationListener, source, checkInterval)
	case class Reconfigured(configuration: Seq[Application])
	private object CheckForConfigurationChanges
}
