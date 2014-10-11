package easyrider.business.ssh

import easyrider.Commands.Success
import easyrider.Repository.Version
import easyrider._
import easyrider.Infrastructure.NodeId

object SshInfrastructure {
  case class NodeConfiguration(id: NodeId, host: String, port: Int, login: String, password: String)
  trait SshInfrastructureCommand extends Command
  case class CreateNode(commandDetails: CommandDetails, nodeConfiguration: NodeConfiguration) extends SshInfrastructureCommand
  case class UpdateNode(commandDetails: CommandDetails, nodeConfiguration: NodeConfiguration) extends SshInfrastructureCommand
  case class RemoveNode(commandDetails: CommandDetails, nodeId: NodeId, keepData: Boolean = true) extends SshInfrastructureCommand
  case class RunSshCommand(commandDetails: CommandDetails, nodeId: NodeId, command: String) extends SshInfrastructureCommand
  case class SftpUploadCommand(commandDetails: CommandDetails, version: Version, targetFolder: String, targetFileName: String) extends SshInfrastructureCommand

  case class NodeConfigurationUpdatedEvent(eventDetails: EventDetails, nodeConfiguration: NodeConfiguration,
                                           captureOutput: Boolean = false) extends Event
  case class RunSshCommandSuccess(eventDetails: EventDetails, output: Option[String]) extends Success
  case class SftpUploadCommandSuccess(eventDetails: EventDetails) extends Success
}
