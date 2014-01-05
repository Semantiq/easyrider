package eu.semantiq.easyrider

import akka.actor.ActorSystem
import java.io.File

object Main extends App {
  private def configFileLocation: File = new File(System.getProperty("configuration", "configuration.json"))
  private def workingDirectory: File = new File(System.getProperty("working.directory", "working"))
  val system = ActorSystem("Easyrider")
  val leader = system.actorOf(LeadingActor(configFileLocation, workingDirectory), "leader")
  system.awaitTermination()
}
