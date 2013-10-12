package pakka.protocol.network.ip

import pakka.protocol.network.NetworkProtocolHeader
import pakka.util.Unsigned

trait InternetProtocolHeader[Address <: InternetAddress] extends NetworkProtocolHeader 
{
	def version : ProtocolVersion
	def source : Address
	def destination : Address
	def nextHeader : NextHeader
	def trafficClass : TrafficClass
	def packetLength : Int
}


final case class ProtocolVersion(toByte : Byte) extends AnyVal


final case class NextHeader(toByte : Byte) extends AnyVal with Unsigned.ByteWrapper


final case class TrafficClass(toByte : Byte) extends AnyVal with Unsigned.ByteWrapper
{
	def differentiatedServicesCodePoint = toInt >> 2
	def typeOfService = differentiatedServicesCodePoint
	def ecnCodePoint = toInt & 0x3
	def ecnCapableTransport : Boolean = ecnCodePoint != 0
	def congestionEncountered : Boolean = ecnCodePoint == 3
}

