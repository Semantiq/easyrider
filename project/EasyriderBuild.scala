import sbt._
import Keys._

object EasyriderBuild extends Build {
  override val settings = Seq(
    scalaVersion := "2.11.0",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" % "akka-actor_2.10" % "2.3.5")
  )

  lazy val root = Project(
    id = "easyrider",
    base = file("."),
    settings = Project.defaultSettings ++ settings ++ super.settings)
}
