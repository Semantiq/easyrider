package easyrider.hardware

object Main extends App {
  args.toSeq match {
    case Seq("create", workingDirectory) =>
      create(workingDirectory)
    case Seq("run", workingDirectory) =>
      run(workingDirectory)
    case Seq("--help") =>
      help
    case Seq("help") =>
      help
    case Seq("--version") =>
      version
    case Seq("version") =>
      version
    case x =>
      println("Unrecognized arguments: " + x.mkString(" "))
      println("Type easyrider --help to get more informations")
  }

  def create(workingDirectory: String) {
    println("Going to create new hardware cluster in " + workingDirectory)
  }

  def run(workingDirectory: String) {
    println("Going to run hardware cluster located in " + workingDirectory)
    val node = new HardwareNode(workingDirectory)
    node.start
    node.join
  }

  def help {
    println("""EasyRider - Run apps straight from Git, with autorefresh
http://easyrider.com
Commands:
""" + "\t" + """create <dir> - Creates new hardware cloud working directory with new private key and config.
""".stripMargin)
  }

  def version {
    println("easyrider version 0.0.1")
  }
}
