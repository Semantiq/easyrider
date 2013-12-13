package eu.semantiq.easyrider

import java.io.File
import org.apache.commons.io.FileUtils._

class DummyConfigFile {
  val location = new File("target/test-configuration.json")
  copyFile(new File("src/test/resources/test-configuration.json"), location)

  def update(configuration: String) {
    writeStringToFile(location, configuration)
  }
}
