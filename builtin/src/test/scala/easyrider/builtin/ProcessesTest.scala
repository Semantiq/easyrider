package easyrider.builtin

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import akka.util.Timeout
import easyrider.Commands.{Failure, Success}
import easyrider.RemoteAccess.{RunRemoteCommand, RunRemoteCommandSuccess}
import easyrider._
import org.scalatest.{FlatSpecLike, Matchers}

class ProcessesTest extends TestKit(ActorSystem()) with FlatSpecLike with Matchers with ImplicitSender {
  val timeout = Timeout(1, TimeUnit.SECONDS)
  implicit val dispatcher = system.dispatcher

  "SequentialProcess" should "be successful given all steps are successful" in {
    val runner = TestProbe()

    SequentialProcess(system)(
      CommandMonitor(system)(runner.ref, RunRemoteCommand(CommandDetails(), NodeId("n1"), "command1")),
      CommandMonitor(system)(runner.ref, RunRemoteCommand(CommandDetails(), NodeId("n1"), "command2")))
    .onSuccess(success => self ! success)
    .run()

    respondWithSuccess(runner)
    respondWithSuccess(runner)

    expectMsgClass(classOf[Success])
  }

  it should "fail and stop, given a step fails" in {
    val runner = TestProbe()

    SequentialProcess(system)(
      CommandMonitor(system)(runner.ref, RunRemoteCommand(CommandDetails(), NodeId("n1"), "faultyCommand")),
      CommandMonitor(system)(runner.ref, RunRemoteCommand(CommandDetails(), NodeId("n1"), "command2")))
    .onFailure(failure => self ! failure)
    .run()

    respondWithFailure(runner)
    runner.expectNoMsg()

    expectMsgClass(classOf[Failure])
  }

  "ParallelProcess" should "run tasks in parallel and finish when all finished" in {
    val runner = TestProbe()

    ParallelProcess(system)(
      successes => successes.head,
      CommandMonitor(system)(runner.ref, RunRemoteCommand(CommandDetails(), NodeId("n1"), "command1")),
      CommandMonitor(system)(runner.ref, RunRemoteCommand(CommandDetails(), NodeId("n1"), "command2")))
    .onSuccess(success => self ! success)
    .run()

    val command1 = runner.expectMsgClass(classOf[RunRemoteCommand])
    val command1sender = runner.lastSender
    val command2 = runner.expectMsgClass(classOf[RunRemoteCommand])
    val command2sender = runner.lastSender

    expectNoMsg()
    command1sender ! RunRemoteCommandSuccess(EventDetails(EventId.generate(), EventKey(), Seq()), None, command1.commandDetails.commandId)
    expectNoMsg()
    command2sender ! RunRemoteCommandSuccess(EventDetails(EventId.generate(), EventKey(), Seq()), None, command2.commandDetails.commandId)

    expectMsgClass(classOf[Success])
  }

  it should "should terminate immediately on task failure" in {
    val runner = TestProbe()

    ParallelProcess(system)(
      successes => successes.head,
      CommandMonitor(system)(runner.ref, RunRemoteCommand(CommandDetails(), NodeId("n1"), "faultyCommand")),
      CommandMonitor(system)(runner.ref, RunRemoteCommand(CommandDetails(), NodeId("n1"), "command2")))
    .onFailure(failure => self ! failure)
    .run()

    val command1 = runner.expectMsgClass(classOf[RunRemoteCommand])
    val command1sender = runner.lastSender
    val command2 = runner.expectMsgClass(classOf[RunRemoteCommand])

    expectNoMsg()
    command1sender ! command1.failure("Something went wrong")
    expectMsgClass(classOf[Failure])
  }

  def respondWithSuccess(runner: TestProbe) = {
    val command = runner.expectMsgClass(classOf[AnyRef]).asInstanceOf[RunRemoteCommand]
    runner.lastSender ! RunRemoteCommandSuccess(EventDetails(EventId.generate(), EventKey(), Seq()), None, command.commandDetails.commandId)
  }

  def respondWithFailure(runner: TestProbe) = {
    val command = runner.expectMsgClass(classOf[AnyRef]).asInstanceOf[RunRemoteCommand]
    runner.lastSender ! command.failure("Simulated failure")
  }
}
