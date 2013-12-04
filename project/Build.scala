import sbt._
import Keys._

object EasyriderBuild extends Build {
  override val settings = Seq(
    scalaVersion := "2.10.3",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" % "akka-actor_2.10" % "2.2.0")
  )

  lazy val root = Project(
    id = "easyrider",
    base = file("."),
    settings = Project.defaultSettings ++ settings ++ super.settings)
}
