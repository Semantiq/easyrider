package eu.semantiq.easyrider

import java.io.File
import org.apache.commons.io.FileUtils

class DummyWorkingCopy {
  val location = new File("target/dummy-working-copy")
  location.mkdir()
  FileUtils.writeStringToFile(new File(location, "failing.sh"), "#!/bin/sh\nexit 1\n")
  FileUtils.writeStringToFile(new File(location, "timing-out.sh"), "#!/bin/sh\nsleep 10\n")
  FileUtils.writeStringToFile(new File(location, "ok.sh"), "#!/bin/sh\nexit 0\n")
}
