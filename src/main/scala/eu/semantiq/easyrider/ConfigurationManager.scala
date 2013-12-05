package eu.semantiq.easyrider

import akka.actor._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import java.io.File

class ConfigurationManager(configurationListener: ActorRef, source: File) extends Actor {
  import ConfigurationManager._
  implicit val formats = DefaultFormats

  configurationListener ! Reconfigured(configuration)

  def receive: Receive = Actor.emptyBehavior

  private def configuration: Seq[Application] = parse(source).extract[Seq[Application]]
}

object ConfigurationManager {
  case class Reconfigured(configuration: Seq[Application])
}