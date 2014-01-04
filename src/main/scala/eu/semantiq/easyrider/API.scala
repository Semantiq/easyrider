package eu.semantiq.easyrider

case class GitRepositoryRef(url: String, branch: String)
case class Compilation(command: Option[String], distributionFolder: String)
case class Running(command: String)

case class PackageMetadata(compilation: Compilation, running: Running)

case class Application(name: String, repository: GitRepositoryRef, settings: Map[String, String] = Map.empty)
