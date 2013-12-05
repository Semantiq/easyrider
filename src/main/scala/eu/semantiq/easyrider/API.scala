package eu.semantiq.easyrider

case class GitRepositoryRef(url: String, branch: String)
case class Commands(run: String, compile: Option[String] = None)

case class Application(name: String, repository: GitRepositoryRef, commands: Commands,
                       settings: Map[String, String] = Map())
