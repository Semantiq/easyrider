package eu.semantiq.easyrider

import java.io.File
import org.apache.commons.io.FileUtils.{copyFile, writeStringToFile}
import org.json4s._
import org.json4s.jackson.Serialization._
import org.apache.commons.io.FileUtils

class DummyConfigFile(id: String) {
  private implicit val formats = DefaultFormats
  val location = new File(s"target/test-configuration-$id.json")
  copyFile(new File("src/test/resources/test-configuration.json"), location)

  def update(configuration: String) {
    writeStringToFile(location, configuration)
  }

  def update(configuration: Seq[Application]) {
    val serialized: String = write(configuration)
    FileUtils.writeStringToFile(location, serialized)
  }
}
