import sbt._
import Keys._
import spray.revolver.RevolverPlugin._

object EasyriderBuild extends Build {
  val akkaVersion = "2.3.5"
  override val settings = Seq(
    scalaVersion := "2.10.4",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "org.json4s" %% "json4s-native" % "3.2.10",
      "com.wandoulabs.akka" %% "spray-websocket" % "0.1.2",
      "org.scalatest" %% "scalatest" % "2.2.1" % "test",
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test")
  )

  lazy val root = hardwareClient
  //Project(id = "easyrider", base = file("."), settings = Project.defaultSettings) aggregate (hardwareClient)

  /*lazy val hardware = Project(
    id = "hardware",
    base = file("hardware"),
    settings = Project.defaultSettings ++ settings ++ super.settings) dependsOn(hardwareClient)*/

  lazy val hardwareClient = Project(
    id = "hardwareClient",
    base = file("hardwareClient"),
    settings = Project.defaultSettings ++ Revolver.settings ++ settings ++ super.settings)

  /*lazy val business = Project(
    id = "business",
    base = file("business"),
    settings = Project.defaultSettings ++ settings ++ super.settings)*/
}
