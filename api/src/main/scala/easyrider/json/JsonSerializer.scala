package easyrider.json

import easyrider.Applications.{ApplicationUpdatedEvent, ApplicationId, CreateApplication}
import easyrider._
import org.json4s.JsonAST.JString
import org.json4s._
import org.json4s.ext.JodaTimeSerializers
import org.json4s.native.Serialization

class JsonSerializer {
  private implicit val formats = Serialization.formats(ShortTypeHints) ++
    Seq(IdentifierSerializer, EventKeySerializer) ++
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

  private object ShortTypeHints extends TypeHints {
    private val typeForHint = Map[String, Class[_]](
      "CreateApplication" -> classOf[CreateApplication],
      "ApplicationUpdatedEvent" -> classOf[ApplicationUpdatedEvent]
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
      case id: Identifier[_] => JString(id.id)
    }
  }

  private object EventKeySerializer extends Serializer[EventKey] {
    private val EventKeyClass = classOf[EventKey]
    private val separator = ":"

    override def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), EventKey] = {
      case (TypeInfo(EventKeyClass, _), JString(eventKeyString))  =>
        EventKey(eventKeyString.split(separator) :_*)
    }
    override def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case eventKey: EventKey => JString(eventKey.key.mkString(separator))
    }
  }

}
