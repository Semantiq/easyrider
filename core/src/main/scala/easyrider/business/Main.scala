package easyrider.business

import java.io.{File, FileReader}
import java.util.Properties

object Main extends App {
  private val properties = new Properties()
  // TODO: clean-up, use some other configuration library
  try {
    properties.load(new FileReader("etc/config.properties"))
  } catch {
    case e: Exception => // ignore
  }
  val easyRider = new EasyRider(properties.getProperty("port", "8080").toInt, new File("data"))
}
