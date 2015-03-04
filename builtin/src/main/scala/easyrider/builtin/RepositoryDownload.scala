package easyrider.builtin

import java.io.{File, RandomAccessFile}
import java.nio.ByteBuffer

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import akka.util.ByteString
import easyrider.Repository.{Ack, UploadChunk, UploadCompleted, Version}

class RepositoryDownload(version: Version, target: ActorRef, val easyriderData: File) extends Actor with RepositoryDirectoryLayout {
  private val versionFile = new RandomAccessFile(new File(repositoryDir, version.applicationId.id + "/" + version.number + ".tar.bz2"), "r")
  private val channel = versionFile.getChannel
  context.watch(target)
  readChunk()

  private def readChunk() {
    val buffer = ByteBuffer.allocate(1024 * 128)
    val bytesRead = channel.read(buffer)
    if (bytesRead > 0) {
      val byteString = ByteString.fromArray(buffer.array(), 0, bytesRead)
      target ! UploadChunk(byteString)
    } else {
      target ! UploadCompleted()
      versionFile.close()
      context.stop(self)
    }
  }

  override def receive: Receive = LoggingReceive {
    case Ack => readChunk()
  }
}

object RepositoryDownload {
  def apply(easyriderData: File)(version: Version, target: ActorRef) = Props(classOf[RepositoryDownload], version, target, easyriderData)
}
