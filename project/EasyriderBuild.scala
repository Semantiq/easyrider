import sbt._
import Keys._
import spray.revolver.RevolverPlugin._

object EasyriderBuild extends Build {
  val akkaVersion = "2.3.5"
  override val settings = Seq(
    scalaVersion := "2.10.4",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
      "ch.qos.logback" % "logback-classic" % "1.0.0" % "runtime",
      "org.json4s" %% "json4s-native" % "3.2.10",
      "org.json4s" %% "json4s-ext" % "3.2.10",

      /*
      git clone https://github.com/mateuszj/spray-websocket.git
      cd spray-websocket
      git checkout v0.1.2_2.11
      sbt publishLocal
      */

      "com.wandoulabs.akka" %% "spray-websocket" % "0.1.2-SEMANTIQ",

      "commons-io" % "commons-io" % "2.4",
      "com.jcraft" % "jsch" % "0.1.51",
      "joda-time" % "joda-time" % "2.4",
      "org.scalatest" %% "scalatest" % "2.2.1" % "test",
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test")
  )

  lazy val root = Project(
    id = "root",
    base = file("hardwareClient"),
    settings = Project.defaultSettings ++ Revolver.settings ++ settings ++ super.settings)
  //Project(id = "easyrider", base = file("."), settings = Project.defaultSettings) aggregate (hardwareClient)

  /*lazy val hardware = Project(
    id = "hardware",
    base = file("hardware"),
    settings = Project.defaultSettings ++ settings ++ super.settings) dependsOn(hardwareClient)*/

  /*lazy val business = Project(
    id = "business",
    base = file("business"),
    settings = Project.defaultSettings ++ settings ++ super.settings)*/
}
