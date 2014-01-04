package eu.semantiq.easyrider

import akka.actor.{Props, Actor}
import java.io.{FileWriter, File}
import org.apache.commons.io.FileUtils
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization._

class AppRepository(storageFolder: File) extends Actor {
  import AppRepository._
  private implicit val formats = DefaultFormats

  storageFolder.mkdirs()

  def receive: Receive = {
    case DeployVersion(app, version, ref, metadata) =>
      val location = packageLocation(app, version)
      ref.extractTo(location)
      saveMetadata(location, metadata)
      context.system.eventStream.publish(VersionAvailable(app, version))
    case GetVersion(app, version) =>
      val location = packageLocation(app, version)
      sender ! GetVersionResponse(app, version, PackageRef.fromFolder(location), readMetadata(location))
  }

  private def packageLocation(app: String, version: String) = {
    val appLocation = new File(storageFolder, app)
    appLocation.mkdir()
    val versionLocation = new File(appLocation, version)
    versionLocation
  }
  private def saveMetadata(location: File, metadata: PackageMetadata) {
    val w = new FileWriter(new File(location, ".easyrider.json"))
    try {
      write(metadata, w)
    } finally {
      w.close()
    }
  }
  private def readMetadata(location: File) = {
    parse(new File(location, ".easyrider.json")).extract[PackageMetadata]
  }
}

object AppRepository {
  def apply(storageFolder: File) = Props(classOf[AppRepository], storageFolder)
  case class DeployVersion(app: String, version: String, appPackage: PackageRef, packageMetadata: PackageMetadata)
  case class GetVersion(app: String, version: String)
  case class GetVersionResponse(app: String, version: String, packageRef: PackageRef, packageMetadata: PackageMetadata)
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