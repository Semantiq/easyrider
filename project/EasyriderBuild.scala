import sbt._
import Keys._

object EasyriderBuild extends Build {
  private val akkaVersion = "2.2.3"

  override val settings = Seq(
    scalaVersion := "2.10.3",
    libraryDependencies ++= Seq(
      // main
      "com.typesafe.akka" % "akka-actor_2.10" % akkaVersion,
      "org.json4s" %% "json4s-jackson" % "3.2.4",
      "org.apache.commons" % "commons-io" % "1.3.2",
      // test
      "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
      "com.typesafe.akka" % "akka-testkit_2.10" % akkaVersion % "test"
    )
  )

  lazy val root = Project(
    id = "easyrider",
    base = file("."),
    settings = Project.defaultSettings ++ settings ++ super.settings)
}
