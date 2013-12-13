package eu.semantiq.easyrider

import akka.actor.{Props, ActorRef, Actor}
import java.io.File

class Compiler(listener: ActorRef, workingDir: File, command: Some[String]) extends Actor {
  def receive: Actor.Receive = ???
}

object Compiler {
  def apply(listener: ActorRef, workingDir: File, command: Some[String]) = Props(classOf[Compiler], listener, workingDir, command)
  object CompilationSuccessful
}
