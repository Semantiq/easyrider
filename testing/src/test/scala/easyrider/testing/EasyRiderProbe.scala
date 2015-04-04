package easyrider.testing

import akka.actor.{Actor, ActorRef, Props}
import easyrider.Command
import easyrider.json.JsonSerializer
import spray.can.websocket.Send
import spray.can.websocket.frame.TextFrame

object EasyRiderProbe {
  private class EasyRiderProbe(master: ActorRef) extends Actor {
    val socket = context.actorOf(WebSocketProbe(self), "socket")
    val serializer = new JsonSerializer()

    override def receive: Receive = {
      case command: Command =>
        val string = serializer.write(command)
        println(">> " + string)
        socket ! Send(TextFrame(string))
      case frame: TextFrame =>
        val string = frame.payload.utf8String
        println("<< " + string)
        val event = serializer.readEvent(string)
        master ! event
    }
  }

  def apply(master: ActorRef) = Props(new EasyRiderProbe(master))
}
