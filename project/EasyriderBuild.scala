import sbt._
import Keys._

object EasyriderBuild extends Build {
  val akkaVersion = "2.3.5"
  override val settings = Seq(
    scalaVersion := "2.11.0",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "org.scalatest" %% "scalatest" % "2.2.1" % "test",
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test")
  )

  lazy val root = Project(id = "easyrider", base = file(".")) aggregate (hardwareClient)

  lazy val hardware = Project(
    id = "hardware",
    base = file("hardware"),
    settings = Project.defaultSettings ++ settings ++ super.settings) dependsOn(hardwareClient)

  lazy val hardwareClient = Project(
    id = "hardwareClient",
    base = file("hardwareClient"),
    settings = Project.defaultSettings ++ settings ++ super.settings)

  lazy val business = Project(
    id = "business",
    base = file("business"),
    settings = Project.defaultSettings ++ settings ++ super.settings)
}
