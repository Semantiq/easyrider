package easyrider.business

import java.io.FileReader
import java.util.Properties

object Configuration {
  private val properties = new Properties()
  // TODO: clean-up, use some other configuration library
  try {
    properties.load(new FileReader("etc/config.properties"))
  } catch {
    case e: Exception => // ignore
  }
  val port: Int = properties.getProperty("port", "8080").toInt
  val builtinPasswordHash = properties.getProperty("builtin.passwordHash", "disabled-by-default")
}
