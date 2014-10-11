package easyrider.business.core

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import easyrider.Api.CommandSentEvent
import easyrider.Commands.Success
import easyrider._
import org.scalatest.{FlatSpecLike, Matchers}

class CommandCenterTest extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ImplicitSender {
  case class DummyCommand(commandDetails: CommandDetails) extends Command
  case class DummySuccess(eventDetails: EventDetails) extends Success

  "CommandCentre" should "allow providers to register and execute commands" in {
    val (eventBus, commandCenter, provider) = setup()

    commandCenter ! DummyCommand(CommandDetails(CommandId("1"), TraceMode()))

    provider.expectMsgClass(classOf[DummyCommand])
    eventBus.expectMsgClass(classOf[CommandSentEvent])
  }

  it should "send executions to caller" in {
    val (eventBus, commandCenter, provider) = setup()

    commandCenter ! DummyCommand(CommandDetails(CommandId("1"), TraceMode(progress = true, confirmation = true)))

    provider.expectMsgClass(classOf[DummyCommand])
    provider.reply(DummySuccess(EventDetails(EventId.generate(), EventKey(), Seq(CommandId("1")))))

    eventBus.expectMsgClass(classOf[CommandSentEvent])
    eventBus.expectMsgClass(classOf[DummySuccess])
  }

  it should "stop sending executions after ignoring execution stream" in {

  }

  def setup() = {
    val eventBus = TestProbe()
    val commandCenter = system.actorOf(CommandCenter(eventBus.ref))
    val provider = TestProbe()
    provider.send(commandCenter, Commands.RegisterProvider(classOf[DummyCommand]))
    (eventBus, commandCenter, provider)
  }
}
