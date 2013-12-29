package eu.semantiq.easyrider.builder

import java.io.File
import scala.sys.process._
import org.apache.commons.io.FileUtils
import eu.semantiq.easyrider.GitRepositoryRef

class DummyGitRepository(name: String) {
  private val folder = new File(s"target/$name")

  FileUtils.deleteDirectory(folder)
  run(s"git init ${folder.getAbsolutePath}", folder.getParentFile)
  updateFile("run.sh", "#!/bin/sh\n\necho \"Hello!\"\n")

  def updateFile(name: String, content: String) {
    FileUtils.writeStringToFile(new File(folder, name), content)
    run("git add -A", folder)
    run("git commit -m test", folder)
  }

  def gitURL = GitRepositoryRef(s"file://${folder.getAbsolutePath}", "master")

  private def run(command: String, dir: File) {
    val code = Process(command, dir, "PATH" -> System.getenv("PATH")) !;
    assert(code == 0, s"$command returned code $code")
  }
}
