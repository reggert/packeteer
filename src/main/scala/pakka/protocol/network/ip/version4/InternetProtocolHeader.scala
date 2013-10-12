package pakka.protocol.network.ip.version4

import pakka.protocol.network.ip
import pakka.util.Unsigned


trait InternetProtocolHeader extends ip.InternetProtocolHeader[InternetAddress] 
{
	override final def version = ip.ProtocolVersion(4)
	
	def headerLength : InternetHeaderLength
}


final case class InternetHeaderLength(words : Byte) extends AnyVal
{
	def bytes = words * 4
}


final case class Identification(toShort : Short) extends AnyVal with Unsigned.ShortWrapper