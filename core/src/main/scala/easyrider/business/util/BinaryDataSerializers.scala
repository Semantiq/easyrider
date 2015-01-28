package easyrider.business.util

import easyrider.BinaryData
import org.json4s.JsonAST.{JInt, JString, JObject}
import org.json4s.{TypeInfo, JValue, Formats, Serializer}

object BinaryDataSerializers {
  def short = List(new Serializer[BinaryData] {
    override def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), BinaryData] = ???
    override def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case BinaryData(data) => JObject(
        ("jsonClass", JString("easyrider.BinaryData")),
        ("size", JInt(data.size)))
    }
  })
}
