package easyrider

import akka.util.ByteString

case class BinaryData(data: ByteString) {
  override def toString = s"<${data.size} bytes>"
}
