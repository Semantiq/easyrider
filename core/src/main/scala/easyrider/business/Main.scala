package easyrider.business

import java.io.File

object Main extends App {
  val easyRider = new EasyRider(Configuration.port, new File("data"))
}
