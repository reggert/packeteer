package pakka.protocol.network.ip.version4

import pakka.protocol.network.ip
import pakka.util.Unsigned


trait InternetProtocolHeader extends ip.InternetProtocolHeader[InternetAddress] 
{
	override final def version = ip.ProtocolVersion(4)
	
	def headerLength : InternetHeaderLength
	def totalLength : Int
	final def timeToLive = hopLimit
	def identification : Identification
	def flags : Flags
	def fragmentOffset : Short
	def options : Seq[HeaderOption]
}


final case class InternetHeaderLength(toByte : Byte) extends AnyVal with Unsigned.ByteWrapper
{
	def words = toInt
	def bytes = words * 4
}


final case class Identification(toShort : Short) extends AnyVal with Unsigned.ShortWrapper


final case class Flags(reserved : Boolean, dontFragment : Boolean, moreFragments : Boolean) 


trait HeaderOption
{
	def optionType : OptionType
	def optionLength : Int
}


final case class OptionType(toByte : Byte) extends AnyVal
{
	def copied : Boolean = (toByte & 0x80) != 0
	def optionClass : Int = (toByte & 0x60) >> 5
	def optionNumber : Int = (toByte & 0x1f)
}

