package easyrider.business

import java.io.File

object Main extends App {
  val easyRider = new EasyRider(8080, new File("/opt/easyrider"))
}
