package easyrider.business.ssh

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import akka.util.ByteString
import easyrider.{RemoteAccess, CommandDetails}
import easyrider.Infrastructure.NodeId
import easyrider.Repository._

class BuiltInPackageUpload(sshSession: ActorRef, repository: ActorRef) extends Actor with ActorLogging {
  import easyrider.business.ssh.BuiltInPackageUpload._

  var caller: ActorRef = _

  def initializing() = LoggingReceive {
    case command @ Upload(version, nodeId, packageFolder, packageFile) =>
      repository ! StartDownload(version)
      caller = sender()
      context.become(initializingDownload(command))
  }

  def initializingDownload(command: Upload) = LoggingReceive {
    case UploadChunk(data) =>
      sshSession ! RemoteAccess.StartUpload(CommandDetails(), command.nodeId, command.packageFolder, command.packageFile)
      context.become(initializingUpload(command, sender(), data))
  }

  def initializingUpload(command: Upload, download: ActorRef, data: ByteString) = LoggingReceive {
    case RemoteAccess.UploadNextChunk(_, uploadId) =>
      download ! Ack
      sshSession ! RemoteAccess.UploadChunk(CommandDetails(), command.nodeId, uploadId, data)
      context.become(uploading(command, download, uploadId, Seq(), 0))
  }

  def uploading(command: Upload, download: ActorRef, uploadId: String, queue: Seq[ByteString], permissions: Int): Receive = LoggingReceive {
    case UploadChunk(data) if permissions == 0 =>
      context.become(uploading(command, download, uploadId, queue :+ data, 0))
    case UploadChunk(data) if permissions > 0 =>
      sshSession ! RemoteAccess.UploadChunk(CommandDetails(), command.nodeId, uploadId, data)
      context.become(uploading(command, download, uploadId, queue, permissions - 1))
    case RemoteAccess.UploadNextChunk(_, currentUploadId) if queue.nonEmpty =>
      download ! Ack
      sshSession ! RemoteAccess.UploadChunk(CommandDetails(), command.nodeId, uploadId, queue.head)
      context.become(uploading(command, download, uploadId, queue.tail, 0))
    case RemoteAccess.UploadNextChunk(_, currentUploadId) if queue.isEmpty =>
      context.become(uploading(command, download, uploadId, queue, permissions + 1))
    case UploadCompleted() =>
      for (data <- queue) {
        sshSession ! RemoteAccess.UploadChunk(CommandDetails(), command.nodeId, uploadId, data)
      }
      sshSession ! RemoteAccess.UploadComplete(CommandDetails(), command.nodeId, uploadId)
      caller ! UploadComplete()
      context.stop(self)
  }

  override def receive = initializing()
}

object BuiltInPackageUpload {
  def apply(sshSession: ActorRef, repository: ActorRef)() = Props(classOf[BuiltInPackageUpload], sshSession, repository)

  case class Upload(version: Version, nodeId: NodeId, packageFolder: String, packageFile: String)
  case class UploadComplete()
}
