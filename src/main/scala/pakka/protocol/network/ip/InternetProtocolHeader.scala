package pakka.protocol.network.ip

import pakka.protocol.network.NetworkProtocolHeader

trait InternetProtocolHeader[Address <: InternetAddress] extends NetworkProtocolHeader 
{
	def version : ProtocolVersion
	def source : Address
	def destination : Address
	def nextHeader : NextHeader
}


final case class ProtocolVersion(toByte : Byte) extends AnyVal


final case class NextHeader(toByte : Byte) extends AnyVal
{
	def toInt = toByte & 0xff
}

