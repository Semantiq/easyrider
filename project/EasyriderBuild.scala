import com.typesafe.sbt.SbtNativePackager._
import spray.revolver.RevolverPlugin._
import com.typesafe.sbt.packager.Keys._
import sbt._
import Keys._

object EasyriderBuild extends Build {
  private val akkaVersion = "2.2.3"

  override val settings = Seq(
    version := "0.3",
    organization := "eu.semantiq",
    scalaVersion := "2.10.3",
    libraryDependencies ++= Seq(
      // main
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
      "ch.qos.logback" % "logback-classic" % "1.0.0" % "runtime",
      "org.json4s" %% "json4s-jackson" % "3.2.4",
      "commons-io" % "commons-io" % "2.4",
      // test
      "org.scalatest" %% "scalatest" % "2.0" % "test",
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test"
    ),
    fork in Test := true
  )

  val debianSettings = packageArchetype.java_application ++ Seq(
    maintainer := "SemantiQ",
    packageDescription := "A simple tool to run application straight from Git easily",
    mappings in Universal += file("src/main/debian/easyrider.sh") -> "../../../etc/init.d/easyrider"
  )  

  lazy val root = Project(
    id = "easyrider",
    base = file("."),
    settings = Project.defaultSettings ++ Revolver.settings ++ settings ++ debianSettings ++ super.settings)
      .dependsOn(uri("git://github.com/lihaoyi/SprayWebSockets.git"))
}
