package easyrider.business.ssh

import easyrider.Infrastructure.NodeId
import easyrider._

object SshInfrastructure {
  case class NodeConfiguration(id: NodeId, host: String, port: Int, login: String, password: String)
  trait SshInfrastructureCommand extends Command
  case class CreateNode(commandDetails: CommandDetails, nodeConfiguration: NodeConfiguration) extends SshInfrastructureCommand
  case class UpdateNode(commandDetails: CommandDetails, nodeConfiguration: NodeConfiguration) extends SshInfrastructureCommand
  case class RemoveNode(commandDetails: CommandDetails, nodeId: NodeId, keepData: Boolean = true) extends SshInfrastructureCommand

  case class NodeConfigurationUpdatedEvent(eventDetails: EventDetails, nodeConfiguration: NodeConfiguration,
                                           captureOutput: Boolean = false,
                                           var snapshotUpdate: SnapshotUpdateDetails[NodeConfiguration] = null) extends Event with SnapshotUpdate[NodeConfiguration] {
    snapshotUpdate = SnapshotUpdateDetails(SnapshotEntryType(classOf[NodeConfiguration]), EventKey(nodeConfiguration.id.id), if (eventDetails.removal) None else Some(nodeConfiguration))
  }

}
