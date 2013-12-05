package eu.semantiq.easyrider

import akka.actor.{Props, ActorSystem}

object Main extends App {
  val system = ActorSystem("Easyrider")
  val leader = system.actorOf(Props[LeadingActor], "leader")
  leader ! LeadingActor.Start
  system.awaitTermination()
}
