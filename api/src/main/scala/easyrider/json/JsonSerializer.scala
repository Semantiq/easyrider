package easyrider.json

import easyrider.Applications.{Application, ApplicationUpdatedEvent, ApplicationId, CreateApplication}
import easyrider._
import org.json4s.JsonAST.JString
import org.json4s._
import org.json4s.ext.JodaTimeSerializers
import org.json4s.native.Serialization

class JsonSerializer {
  private implicit val formats = Serialization.formats(ShortTypeHints) ++
    Seq(IdentifierSerializer, EventKeySerializer, SnapshotEntryTypeSerializer) ++
    JodaTimeSerializers.all

  def readCommand(string: String): Command = {
    Serialization.read[Command](string)
  }

  def readEvent(string: String): Event = {
    Serialization.read[Event](string)
  }

  def write(command: Command): String = {
    Serialization.write(command)
  }

  def write(event: Event): String = {
    Serialization.write(event)
  }

  private val separator = ":"

  private object ShortTypeHints extends TypeHints {
    private val typeForHint = Map[String, Class[_]](
      "CreateApplication" -> classOf[CreateApplication],
      "ApplicationUpdated" -> classOf[ApplicationUpdatedEvent]
    )

    private val hintForType: Map[Class[_], String] = typeForHint map (_.swap)

    override val hints: List[Class[_]] = List(classOf[Command], classOf[Event])

    override def classFor(hint: String): Option[Class[_]] = typeForHint.get(hint)

    override def hintFor(clazz: Class[_]): String = hintForType(clazz)
  }

  private object IdentifierSerializer extends Serializer[Identifier[_]] {
    override def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Identifier[_]] = {
      case (typeInfo, JString(id)) if classOf[CommandId].isAssignableFrom(typeInfo.clazz) => CommandId(id)
      case (typeInfo, JString(id)) if classOf[EventId].isAssignableFrom(typeInfo.clazz) => EventId(id)
      case (typeInfo, JString(id)) if classOf[ApplicationId].isAssignableFrom(typeInfo.clazz) => ApplicationId(id)
    }
    override def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case id: Identifier[_] => JString(id.eventKey.key.mkString(separator))
    }
  }

  private object EventKeySerializer extends Serializer[EventKey] {
    private val EventKeyClass = classOf[EventKey]

    override def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), EventKey] = {
      case (TypeInfo(EventKeyClass, _), JString(eventKeyString))  =>
        EventKey(eventKeyString.split(separator) :_*)
    }
    override def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case eventKey: EventKey => JString(eventKey.key.mkString(separator))
    }
  }

  private object SnapshotEntryTypeSerializer extends Serializer[SnapshotEntryType[_]] {
    private val entryTypeForName = Map[String, SnapshotEntryType[_]](
      "Application" -> SnapshotEntryType(classOf[Application])
    )

    private val nameForEntryType = entryTypeForName.map(_.swap)

    override def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), SnapshotEntryType[_]] = {
      case (typeInfo, JString(name)) if typeInfo.clazz == classOf[SnapshotEntryType[_]] => entryTypeForName(name)
    }

    override def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case entryType: SnapshotEntryType[_] => JString(nameForEntryType(entryType))
    }
  }
}
