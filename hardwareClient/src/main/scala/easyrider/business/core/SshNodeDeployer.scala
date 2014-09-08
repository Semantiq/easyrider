package easyrider.business.core

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import com.jcraft.jsch.{ChannelSftp, JSch}
import easyrider.Infrastructure._
import easyrider.Repository.{Ack, StartDownload, UploadChunk, UploadCompleted}
import easyrider.SshInfrastructure.NodeConfiguration
import easyrider.{EventDetails, EventId}
import org.apache.commons.io.IOUtils

class SshNodeDeployer(eventBus: ActorRef, repository: ActorRef, configuration: NodeConfiguration, command: DeployVersion) extends Actor with SshNodeDirectoryLayout with ActorLogging {
  private val eventKey = command.containerId.eventKey append command.version.number

  repository ! StartDownload(command.version)
  eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(command.commandId)), command.version, DeploymentInProgress)

  // TODO: this is hack-y
  val Some((session, channel, output)) = notifyOnError(() => {
    val jsch = new JSch()
    val session = jsch.getSession(configuration.login, configuration.host, configuration.port)
    session.setPassword(configuration.password)
    session.setConfig("StrictHostKeyChecking", "no")
    session.connect()
    val channel = session.openChannel("sftp").asInstanceOf[ChannelSftp]
    channel.connect()
    channel.cd(versionsDir(command.containerId))
    val output = channel.put(command.version.number + ".tar.bz2")
    (session, channel, output)
  })

  override def receive: Receive = LoggingReceive {
    case UploadChunk(data) =>
      notifyOnError { () =>
        sender() ! Ack
        IOUtils.copy(data.iterator.asInputStream, output)
      }
    case UploadCompleted() =>
      notifyOnError { () =>
        output.close()
        channel.disconnect()
        session.disconnect()
        eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(command.commandId)), command.version, DeploymentCompleted)
      }
  }

  def notifyOnError[T](block: () => T): Option[T] = {
    try {
      Some(block())
    } catch {
      case ex: Exception =>
        // TODO: check what operation failed and make the message out of that
        log.error(ex, "Deployment failed: {} v {} to {}", command.version.applicationId, command.version.number, configuration.id.id)
        eventBus ! VersionDeploymentProgressEvent(EventDetails(EventId.generate(), eventKey, Seq(command.commandId)), command.version, DeploymentFailed(ex.getMessage))
        context.stop(self)
        None
    }
  }
}

object SshNodeDeployer {
  def apply(eventBus: ActorRef, repository: ActorRef)(configuration: NodeConfiguration, command: DeployVersion) = Props(classOf[SshNodeDeployer], eventBus, repository, configuration, command)
}
