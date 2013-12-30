package eu.semantiq.easyrider

case class GitRepositoryRef(url: String, branch: String)
case class Compilation(command: Option[String], distributionFolder: String)
case class Running(command: String, settings: Map[String, String] = Map())

case class Commands(compilation: Compilation, running: Running)

case class Application(name: String, repository: GitRepositoryRef)
