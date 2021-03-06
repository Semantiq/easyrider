package easyrider.builtin

import akka.actor._
import akka.event.LoggingReceive
import akka.util.ByteString
import easyrider.Commands.Failure
import easyrider.Repository._
import easyrider._

import scala.concurrent.duration._
import scala.language.postfixOps

class BuiltInPackageUpload(sshSession: ActorRef, repository: ActorRef) extends Actor with ActorLogging {
  import easyrider.builtin.BuiltInPackageUpload._

  var caller: ActorRef = _

  def initializing() = LoggingReceive {
    case command@Upload(_, version, nodeId, packageFolder, packageFile) =>
      repository ! StartDownload(version)
      caller = sender()
      context.become(initializingDownload(command))
      context.setReceiveTimeout(10 seconds)
  }

  def initializingDownload(command: Upload) = LoggingReceive {
    case UploadChunk(data) =>
      sshSession ! RemoteAccess.StartUpload(CommandDetails(), command.nodeId, command.packageFolder, command.packageFile)
      context.become(initializingUpload(command, sender(), data))
  }

  def initializingUpload(command: Upload, download: ActorRef, data: ByteString) = LoggingReceive {
    case failure: Failure =>
      caller ! command.failure(s"Package upload failed: ${failure.failureMessage}")
      context.stop(self)
    case RemoteAccess.UploadNextChunk(_, uploadId, _, _) =>
      download ! Ack
      sshSession ! RemoteAccess.UploadChunk(CommandDetails(), command.nodeId, uploadId, BinaryData(data))
      context.become(uploading(command, download, uploadId, Seq(), 0))
  }

  def uploading(command: Upload, download: ActorRef, uploadId: String, queue: Seq[ByteString], permissions: Int): Receive = LoggingReceive {
    case UploadChunk(data) if permissions == 0 =>
      context.become(uploading(command, download, uploadId, queue :+ data, 0))
    case UploadChunk(data) if permissions > 0 =>
      sshSession ! RemoteAccess.UploadChunk(CommandDetails(), command.nodeId, uploadId, BinaryData(data))
      context.become(uploading(command, download, uploadId, queue, permissions - 1))
    case RemoteAccess.UploadNextChunk(_, currentUploadId, _, _) if queue.nonEmpty =>
      download ! Ack
      sshSession ! RemoteAccess.UploadChunk(CommandDetails(), command.nodeId, uploadId, BinaryData(queue.head))
      context.become(uploading(command, download, uploadId, queue.tail, 0))
    case RemoteAccess.UploadNextChunk(_, currentUploadId, _, _) if queue.isEmpty =>
      context.become(uploading(command, download, uploadId, queue, permissions + 1))
      download ! Ack
    case UploadCompleted() =>
      for (data <- queue) {
        sshSession ! RemoteAccess.UploadChunk(CommandDetails(), command.nodeId, uploadId, BinaryData(data))
      }
      sshSession ! RemoteAccess.UploadComplete(CommandDetails(), command.nodeId, uploadId)
      caller ! UploadComplete()
      context.stop(self)
    case ReceiveTimeout =>
      // TODO: propagate error information: context.parent ! command.
      context.stop(self)
  }

  override def receive = initializing()
}

object BuiltInPackageUpload {
  def apply(sshSession: ActorRef, repository: ActorRef)() = Props(classOf[BuiltInPackageUpload], sshSession, repository)

  case class Upload(commandDetails: CommandDetails, version: Version, nodeId: NodeId, packageFolder: String, packageFile: String) extends Command
  case class UploadComplete()
}
