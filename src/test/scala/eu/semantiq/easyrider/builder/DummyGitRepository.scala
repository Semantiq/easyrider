package eu.semantiq.easyrider.builder

import java.io.File
import scala.sys.process._
import org.apache.commons.io.FileUtils
import eu.semantiq.easyrider.GitRepositoryRef

class DummyGitRepository(name: String) {
  private val folder = new File(s"target/$name")

  FileUtils.deleteDirectory(folder)
  run(s"git init ${folder.getAbsolutePath}", folder.getParentFile)
  updateFile("compile.sh", "#!/bin/sh\n\necho \"echo hello\" > run.sh\n")
  updateFile(".easyrider.json",
    """
      |{
      |  "compilation": {
      |    "command": "sh compile.sh",
      |    "distributionFolder": "."},
      |  "running": {
      |    "command": "sh run.sh"
      |  }
      |}
    """.stripMargin)

  def updateFile(name: String, content: String) {
    FileUtils.writeStringToFile(new File(folder, name), content)
    run("git add -A", folder)
    run("git commit -m test", folder)
  }

  def gitURL = GitRepositoryRef(s"file://${folder.getAbsolutePath}", "master")

  private def run(command: String, dir: File) {
    val code = Process(command, dir, "PATH" -> System.getenv("PATH")).!(ProcessLogger(_ => ()))
    assert(code == 0, s"$command returned code $code")
  }
}
