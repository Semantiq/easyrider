import sbt._
import Keys._
import com.typesafe.sbt.packager.Keys._
import com.typesafe.sbt.SbtNativePackager.Universal

object EasyRiderKeys {
  val upload = taskKey[Unit]("Upload a package and it's metadata to EasyRider built-in repository")
  val appName = settingKey[String]("Name to use in EasyRider upload")
  val repositoryUrl = settingKey[String]("EasyRider built-in repository URL")
  val login = settingKey[String]("Built-in repository login")
  val password = settingKey[String]("Built-in repository password")
}

object EasyRiderPlugin extends AutoPlugin {
  import EasyRiderKeys._

  override lazy val projectSettings = Seq(
    appName := name.value,
    upload := {
      val file = packageZipTarball.in(Universal).value
      println(s"Uploading to EasyRider built-in repository: ${appName.value} version ${version.value} (${file.length()} bytes)")
      s"curl -v -include --user ${login.value}:${password.value} -T ${file.absolutePath} ${repositoryUrl.value}/api/repository/upload?application=${appName.value}&version=${version.value}" !
      //val gitData = "git log -n 2" !!
      //println("git says: " + gitData)
    }
  )
}
