package packeteer.protocol.network.ip.version6

import packeteer.protocol.network.ip
import packeteer.util.Unsigned
import scala.collection.immutable

trait InternetProtocolHeader extends ip.InternetProtocolHeader[InternetAddress] 
{
	def payloadLength : Int
	def flowLabel : FlowLabel
	def extensions : Seq[ExtensionHeader]
	def headerChecksum : HeaderChecksum
}


final case class FlowLabel(toInt : Int) extends AnyVal

final case class HeaderChecksum(toShort : Short) extends AnyVal


trait ExtensionHeader
{
	def headerType : ip.HeaderType
	def headerLength : HeaderExtensionLength
}


final case class HeaderExtensionLength(toByte : Byte) extends AnyVal with Unsigned.ByteWrapper
{
	def longWords = toInt
	def bytes = longWords * 8
}