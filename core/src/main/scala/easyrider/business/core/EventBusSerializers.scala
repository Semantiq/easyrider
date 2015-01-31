package easyrider.business.core

import easyrider.EventKey
import org.json4s.JsonAST.JString
import org.json4s.{TypeInfo, JValue, Formats, Serializer}

object EventBusSerializers {
  val serializers = List(
    new Serializer[EventKey] {
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
  )
}
