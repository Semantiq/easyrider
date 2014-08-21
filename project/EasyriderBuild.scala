import sbt._
import Keys._

object EasyriderBuild extends Build {
  override val settings = Seq(
    scalaVersion := "2.11.0",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % "2.3.5")
  )

  lazy val root = Project(id = "easyrider", base = file(".")) dependsOn(hardware, hardwareClient, business)

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
