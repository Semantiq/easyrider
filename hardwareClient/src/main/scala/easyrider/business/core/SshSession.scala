package easyrider.business.core

import java.io.OutputStream
import java.util.concurrent.TimeUnit

import akka.actor._
import akka.event.LoggingReceive
import akka.util.Timeout
import com.jcraft.jsch.{ChannelExec, ChannelSftp, JSch, Session}
import easyrider.Repository.{Ack, StartDownload, UploadChunk, UploadCompleted}
import easyrider.SshInfrastructure._
import easyrider._
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
          sender() ! easyrider.Failure(any.commandId, "Can't connect to SSH", Some(exception))
      }
  }

  def warm(session: Session) = LoggingReceive {
    case command: SftpUploadCommand =>
      val channel = session.openChannel("sftp").asInstanceOf[ChannelSftp]
      channel.connect()
      channel.cd(command.targetFolder)
      val output = channel.put(command.targetFileName)
      repository ! StartDownload(command.version)
      becomeUploading(session, channel, output, command, sender())
    case RunSshCommand(commandId, nodeId, command) =>
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
      sender ! RunSshCommandSuccess(commandId, Some(output))

    case ReceiveTimeout =>
      Try(session.disconnect()) match {
        case Failure(exception) => log.error(exception, "Couldn't disconnect from SSH: {}", configuration.id.id)
        case Success(_) => log.info("Disconnected from SSH: {}", configuration.id.id)
      }
      becomeCold()
  }

  def uploading(session: Session, channel: ChannelSftp, output: OutputStream, command: SftpUploadCommand, requestor: ActorRef) = LoggingReceive {
    case UploadChunk(data) =>
      sender() ! Ack
      IOUtils.copy(data.iterator.asInputStream, output)
    case UploadCompleted() =>
      requestor ! SftpUploadCommandSuccess(command.commandId)
      output.close()
      channel.disconnect()
      becomeWarm(session)
  }

  def becomeCold() {
    context.setReceiveTimeout(Duration.Inf)
    context.become(cold)
  }

  def becomeWarm(session: Session) {
    context.setReceiveTimeout(disconnectTimeout)
    context.become(warm(session))
  }

  def becomeUploading(session: Session, channel: ChannelSftp, output: OutputStream, command: SftpUploadCommand, requestor: ActorRef) {
    context.setReceiveTimeout(Duration.Inf)
    context.become(uploading(session, channel, output, command, requestor))
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
