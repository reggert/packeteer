package packeteer.protocol.network.ip

import packeteer.util.Unsigned


final case class TrafficClass(toByte : Byte) extends AnyVal with Unsigned.ByteWrapper
{
	def differentiatedServicesCodePoint = toInt >> 2
	def typeOfService = TypeOfService(toByte)
	def ecnCodePoint = toInt & 0x3
	def ecnCapableTransport : Boolean = ecnCodePoint != 0
	def congestionEncountered : Boolean = ecnCodePoint == 3
}
