package eu.semantiq.easyrider

import java.io.File
import org.apache.commons.io.FileUtils

class DummyWorkingCopy {
  val location = new File("target/dummy-working-copy")
  location.mkdir()
  FileUtils.writeStringToFile(new File(location, "failing.sh"), "#!/bin/sh\nexit 1\n")
  FileUtils.writeStringToFile(new File(location, "10sec.sh"), "#!/bin/sh\nsleep 10\n")
  FileUtils.writeStringToFile(new File(location, "1sec.sh"), "#!/bin/sh\nsleep 1\n")
  FileUtils.writeStringToFile(new File(location, "ok.sh"), "#!/bin/sh\nexit 0\n")
}
