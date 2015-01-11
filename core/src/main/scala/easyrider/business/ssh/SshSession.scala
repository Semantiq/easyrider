package easyrider.business.ssh

import java.io.OutputStream
import java.util.UUID
import java.util.concurrent.TimeUnit

import akka.actor._
import akka.event.LoggingReceive
import akka.util.Timeout
import com.jcraft.jsch.{ChannelExec, ChannelSftp, JSch, Session}
import easyrider._
import easyrider.business.ssh.SshInfrastructure._
import org.apache.commons.io.IOUtils

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class SshSession(eventBus: ActorRef, repository: ActorRef, configuration: NodeConfiguration) extends Actor with ActorLogging with Stash {
  implicit val timeout = Timeout(10, TimeUnit.SECONDS)
  implicit val dispatcher = context.system.dispatcher
  val disconnectTimeout = Duration(1, TimeUnit.SECONDS)

  override def postRestart(throwable: Throwable): Unit = {
    context.setReceiveTimeout(Duration.Inf)
    context.become(cold)
  }

  def cold = LoggingReceive {
    case any: Command =>
      Try(connect()) match {
        case Success(session) =>
          stash()
          becomeWarm(session)
          unstashAll()
        case Failure(exception) =>
          sender() ! any.failure("Can't connect to SSH", exception)
      }
  }

  def warm(session: Session) = LoggingReceive {
    case command: SftpUploadCommand =>
      val channel = session.openChannel("sftp").asInstanceOf[ChannelSftp]
      channel.connect()
      channel.cd(command.targetFolder)
      val output = channel.put(command.targetFileName)
      val uploadId = UUID.randomUUID().toString
      sender() ! SftpUploadNextChunk(EventDetails(EventId.generate(), EventKey(), Seq(command.commandDetails.commandId)), uploadId)
      becomeUploading(session, channel, output, command, sender(), uploadId)
    case RunSshCommand(commandDetails, nodeId, command) =>
      log.debug("Sending command to {}: {}", configuration.id.id.asInstanceOf[Any], command)
      val shell = session.openChannel("exec").asInstanceOf[ChannelExec]
      shell.setCommand(command)
      val in = shell.getInputStream
      val err = shell.getErrStream
      shell.connect()
      val output = IOUtils.toString(in)
      val outputErr = IOUtils.toString(err)
      val exitStatus = shell.getExitStatus
      shell.disconnect()
      log.debug("Command output: {}", output)
      log.debug("Command error: {}", outputErr)
      log.debug("Command exit code: {}", exitStatus)
      sender ! RunSshCommandSuccess(EventDetails(EventId.generate(), EventKey(commandDetails.commandId.id), Seq(commandDetails.commandId)), Some(output))
    case SftpUpdateFile(commandDetails, nodeId, path, filename, content) =>
      log.debug("Writing {} bytes to {}/{}", content.length, path, filename)
      val channel = session.openChannel("sftp").asInstanceOf[ChannelSftp]
      channel.connect()
      channel.cd(path)
      val output = channel.put(filename)
      IOUtils.copy(content.iterator.asInputStream, output)
      output.close()
      sender ! SftpUpdateFileSuccess(EventDetails(EventId.generate(), EventKey(commandDetails.commandId.id), Seq(commandDetails.commandId)))
      channel.disconnect()
    case ReceiveTimeout =>
      Try(session.disconnect()) match {
        case Failure(exception) => log.error(exception, "Couldn't disconnect from SSH: {}", configuration.id.id)
        case Success(_) => log.info("Disconnected from SSH: {}", configuration.id.id)
      }
      becomeCold()
  }

  def uploading(session: Session, channel: ChannelSftp, output: OutputStream, command: SftpUploadCommand, requestor: ActorRef, currentUploadId: String) = LoggingReceive {
    case SftpUploadChunk(commandDetails, _, uploadId, data) if uploadId == currentUploadId =>
      sender() ! SftpUploadNextChunk(EventDetails(EventId.generate(), EventKey(), Seq(commandDetails.commandId)), currentUploadId)
      IOUtils.copy(data.iterator.asInputStream, output)
    case SftpUploadComplete(commandDetails, _, uploadId) if uploadId == currentUploadId =>
      requestor ! SftpUploadCompleted(EventDetails(EventId.generate(), EventKey(), Seq(commandDetails.commandId)), currentUploadId)
      output.close()
      channel.disconnect()
      becomeWarm(session)
    case _ => stash()
  }

  def becomeCold() {
    context.setReceiveTimeout(Duration.Inf)
    context.become(cold)
  }

  def becomeWarm(session: Session) {
    unstashAll()
    context.setReceiveTimeout(disconnectTimeout)
    context.become(warm(session))
  }

  def becomeUploading(session: Session, channel: ChannelSftp, output: OutputStream, command: SftpUploadCommand, requestor: ActorRef, uploadId: String) {
    context.setReceiveTimeout(Duration.Inf)
    context.become(uploading(session, channel, output, command, requestor, uploadId))
  }

  private def connect() = {
    val jsch = new JSch()
    val session = jsch.getSession(configuration.login, configuration.host, configuration.port)
    session.setPassword(configuration.password)
    session.setConfig("StrictHostKeyChecking", "no")
    session.connect()
    session
  }

  override def receive: Receive = cold
}

object SshSession {
  def apply(eventBus: ActorRef, repository: ActorRef)(configuration: NodeConfiguration) = Props(classOf[SshSession], eventBus, repository, configuration)
}
