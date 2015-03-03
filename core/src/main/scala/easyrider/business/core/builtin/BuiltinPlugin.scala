package easyrider.business.core.builtin

import java.io.File
import java.net.URL

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import easyrider.Commands.{CommandExecution, Failure, Success}
import easyrider.Infrastructure.ContainerCommand
import easyrider.Plugins.RegisterContainerPlugin
import easyrider.Repository.StartUpload
import easyrider.business.http.UploadHandler
import easyrider.{Command, CommandDetails, CommandId, PluginFactory}

class BuiltinPlugin extends Actor with ActorLogging {
  var commands = Map[CommandId, ActorRef]()

  context.parent ! RegisterContainerPlugin(CommandDetails(), "builtin")
  val builtinModule = new BuiltinModule(new File("data"), new URL("http://localhost"), context.system, self, self, self)

  override def receive: Receive = {
    case m if sender() == context.parent => m match {
      case m: CommandExecution =>
        commands(m.executionOf) ! m
        if (m.isInstanceOf[Success] || m.isInstanceOf[Failure]) {
          commands -= m.executionOf
        }
      case c: ContainerCommand =>
        builtinModule.containers ! c
      case other => log.info("TODO: deliver message: {}", other)
    }
    case upload: StartUpload => builtinModule.repositoryStorage.forward(upload)
    case m: Command =>
      commands += m.commandDetails.commandId -> sender()
      context.parent ! m
    case m =>
      context.parent ! m
  }
}

class BuiltinPluginFactory extends PluginFactory {
  def props: Props = Props[BuiltinPlugin]
  override def httpHandler(plugin: ActorRef) = Some(HttpWorker(() => UploadHandler(plugin)))
}
