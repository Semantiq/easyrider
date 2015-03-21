import java.time.format.DateTimeFormatter

import com.typesafe.sbt.SbtNativePackager._
import com.typesafe.sbt.packager.Keys._
import com.typesafe.sbt.packager.universal._
import sbt.Keys._
import sbt._
import spray.revolver.RevolverPlugin._
import EasyRiderKeys._
import java.time._

object EasyriderBuild extends Build {
  val akkaVersion = "2.3.5"
  override val settings = Seq(
    version := Instant.now().atZone(ZoneId.systemDefault()).format(DateTimeFormatter.ofPattern("yyyyMMdd-hhmmss")),
    scalaVersion := "2.10.4",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
      "org.slf4j" % "slf4j-api" % "1.7.7",
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
      "commons-codec" % "commons-codec" % "1.9",
      "com.jcraft" % "jsch" % "0.1.51",
      "joda-time" % "joda-time" % "2.4",
      "org.scalatest" %% "scalatest" % "2.2.1" % "test",
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test")
  )

  lazy val api = Project(
    id = "api",
    base = file("api"),
    settings = Project.defaultSettings ++ Revolver.settings ++ settings ++ super.settings
  )

  lazy val ssh = Project(
    id = "ssh",
    base = file("ssh"),
    settings = Project.defaultSettings ++ Revolver.settings ++ settings ++ super.settings) dependsOn api

  lazy val builtin = Project(
    id = "builtin",
    base = file("builtin"),
    settings = Project.defaultSettings ++ Revolver.settings ++ settings ++ super.settings) dependsOn api

  lazy val core = Project(
    id = "core",
    base = file("core"),
    settings = Project.defaultSettings ++ Revolver.settings ++ settings ++ super.settings ++ packageArchetype.java_application ++
        EasyRiderPlugin.projectSettings ++ Seq(
      mappings in Universal += file("core/src/main/bin/run") -> "bin/run",
      topLevelDirectory := None,
      appName := "easyrider",
      repositoryUrl := "http://localhost:8080",
      login := "admin",
      password := "test"
    )) dependsOn (api, ssh, builtin % "runtime")

  Project(id = "easyrider", base = file("."), settings = Project.defaultSettings)
    .enablePlugins(UniversalPlugin)
    .enablePlugins(EasyRiderPlugin)
    .dependsOn(api, core, ssh, builtin)
}
