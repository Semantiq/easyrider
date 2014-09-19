package easyrider.business.ssh

import easyrider.Repository.Version
import easyrider._
import easyrider.Infrastructure.NodeId

object SshInfrastructure {
  case class NodeConfiguration(id: NodeId, host: String, port: Int, login: String, password: String)
  trait SshInfrastructureCommand extends Command
  case class CreateNode(commandId: CommandId, nodeConfiguration: NodeConfiguration) extends SshInfrastructureCommand
  case class UpdateNode(commandId: CommandId, nodeConfiguration: NodeConfiguration) extends SshInfrastructureCommand
  case class RemoveNode(commandId: CommandId, nodeId: NodeId, keepData: Boolean = true) extends SshInfrastructureCommand
  case class RunSshCommand(commandId: CommandId, nodeId: NodeId, command: String) extends SshInfrastructureCommand
  case class SftpUploadCommand(commandId: CommandId, version: Version, targetFolder: String, targetFileName: String) extends SshInfrastructureCommand

  case class NodeConfigurationUpdatedEvent(eventDetails: EventDetails, nodeConfiguration: NodeConfiguration,
                                           captureOutput: Boolean = false) extends Event
  case class RunSshCommandSuccess(commandId: CommandId, output: Option[String]) extends Success
  case class SftpUploadCommandSuccess(commandId: CommandId) extends Success
}
