package eu.semantiq.easyrider

import akka.actor.{Props, Actor}
import java.io.File
import org.apache.commons.io.FileUtils

class AppRepository(storageFolder: File) extends Actor {
  import AppRepository._
  storageFolder.mkdirs()

  def receive: Receive = {
    case DeployVersion(app, version, ref) =>
      ref.extractTo(packageLocation(app, version))
      context.system.eventStream.publish(VersionAvailable(app, version))
    case GetVersion(app, version) =>
      sender ! PackageRef.fromFolder(packageLocation(app, version))
  }

  private def packageLocation(app: String, version: String) = {
    val appLocation = new File(storageFolder, app)
    appLocation.mkdir()
    val versionLocation = new File(appLocation, version)
    versionLocation
  }
}

object AppRepository {
  def apply(storageFolder: File) = Props(classOf[AppRepository], storageFolder)
  case class DeployVersion(app: String, version: String, appPackage: PackageRef)
  case class GetVersion(app: String, version: String)
  case class VersionAvailable(app: String, version: String)
  trait PackageRef {
    def extractTo(folder: File)
  }

  // TODO: use some other type internally, to allow deployments over network
  object PackageRef {
    def fromFolder(rootFolder: File): PackageRef = FilePackageRef(rootFolder)

    private case class FilePackageRef(root: File) extends PackageRef {
      def extractTo(folder: File) {
        FileUtils.copyDirectory(root, folder)
      }
    }
  }
}