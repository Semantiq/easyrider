package easyrider.json

import easyrider.Applications.{ApplicationUpdatedEvent, Application, ApplicationId, CreateApplication}
import easyrider._
import org.scalatest.{FlatSpec, Matchers}

class JsonSerializerTest extends FlatSpec with Matchers {
  val serializer = new JsonSerializer()

  "CreateApplication" should "have a friendly JSON representation" in {
    val command = CreateApplication(CommandDetails(CommandId("1")), Application(ApplicationId("test"), Seq(Property("basic", "host", "localhost"))))
    val string = serializer.write(command)
    println(string)
    serializer.readCommand(string) should be (command)
  }

  "ApplicationUpdatedEvent" should "have a friendly JSON representation" in {
    val application = Application(ApplicationId("test"), Seq())
    val event = ApplicationUpdatedEvent(EventDetails(EventId("1-1"), EventKey("test"), Seq()),
      executionOf = CommandId("1"),
      snapshotUpdate = SnapshotUpdateDetails(SnapshotEntryType(classOf[Application]), ApplicationId("test").eventKey, Some(application)))
    val string = serializer.write(event)
    println(string)
    serializer.readEvent(string) should be (event)
  }
}
