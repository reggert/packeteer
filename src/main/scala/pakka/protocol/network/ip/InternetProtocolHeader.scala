package pakka.protocol.network.ip

import pakka.protocol.network.NetworkProtocolHeader
import pakka.util.Unsigned

trait InternetProtocolHeader[Address <: InternetAddress] extends NetworkProtocolHeader 
{
	def version : ProtocolVersion
	def source : Address
	def destination : Address
	def nextProtocol : HeaderType
	def trafficClass : TrafficClass
	def hopLimit : HopCount
}


final case class ProtocolVersion(toByte : Byte) extends AnyVal


final case class HeaderType(toByte : Byte) extends AnyVal with Unsigned.ByteWrapper


final case class TrafficClass(toByte : Byte) extends AnyVal with Unsigned.ByteWrapper
{
	def differentiatedServicesCodePoint = toInt >> 2
	def typeOfService = differentiatedServicesCodePoint
	def ecnCodePoint = toInt & 0x3
	def ecnCapableTransport : Boolean = ecnCodePoint != 0
	def congestionEncountered : Boolean = ecnCodePoint == 3
}


final case class HopCount(toByte : Byte) extends AnyVal with Unsigned.ByteWrapper