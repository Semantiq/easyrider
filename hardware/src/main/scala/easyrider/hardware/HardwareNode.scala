package easyrider.hardware

import akka.actor._

class HardwareNode(workingDirectory: String) {
  val system = ActorSystem()
  def start {
    // TODO: Init actors
  }
  def join {
    system.awaitTermination
  }
}
