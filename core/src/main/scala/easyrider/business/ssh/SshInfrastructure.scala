package easyrider.business.ssh

import akka.util.ByteString
import easyrider.Commands.Success
import easyrider._
import easyrider.Infrastructure.NodeId

object SshInfrastructure {
  case class NodeConfiguration(id: NodeId, host: String, port: Int, login: String, password: String)
  trait SshInfrastructureCommand extends Command
  case class CreateNode(commandDetails: CommandDetails, nodeConfiguration: NodeConfiguration) extends SshInfrastructureCommand
  case class UpdateNode(commandDetails: CommandDetails, nodeConfiguration: NodeConfiguration) extends SshInfrastructureCommand
  case class RemoveNode(commandDetails: CommandDetails, nodeId: NodeId, keepData: Boolean = true) extends SshInfrastructureCommand

  case class NodeConfigurationUpdatedEvent(eventDetails: EventDetails, nodeConfiguration: NodeConfiguration,
                                           captureOutput: Boolean = false) extends Event

  trait RemoteAccessCommand extends Command {
    def nodeId: NodeId
  }
  case class RunSshCommand(commandDetails: CommandDetails, nodeId: NodeId, command: String) extends RemoteAccessCommand
  case class SftpUploadCommand(commandDetails: CommandDetails, nodeId: NodeId, targetFolder: String, targetFileName: String) extends RemoteAccessCommand
  case class SftpUploadChunk(commandDetails: CommandDetails, nodeId: NodeId, uploadId: String, data: ByteString) extends RemoteAccessCommand {
    override def toString = s"SftpUploadChunk($commandDetails, $nodeId, $uploadId, <${data.length} bytes>)"
  }
  case class SftpUploadComplete(commandDetails: CommandDetails, nodeId: NodeId, uploadId: String) extends RemoteAccessCommand
  case class SftpUpdateFile(commandDetails: CommandDetails, nodeId: NodeId, path: String, filename: String, content: ByteString) extends RemoteAccessCommand

  trait SshEvent extends Event
  case class RunSshCommandSuccess(eventDetails: EventDetails, output: Option[String]) extends Success with SshEvent
  case class SftpUploadNextChunk(eventDetails: EventDetails, uploadId: String) extends Success with SshEvent
  case class SftpUploadCompleted(eventDetails: EventDetails, uploadId: String) extends Success with SshEvent
  case class SftpUpdateFileSuccess(eventDetails: EventDetails) extends Success with SshEvent
}
