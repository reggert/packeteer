package packeteer.protocol.network.ip

import packeteer.util.Unsigned

trait InternetProtocolHeader[Address <: InternetAddress]  
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

final case class HopCount(toByte : Byte) extends AnyVal with Unsigned.ByteWrapper