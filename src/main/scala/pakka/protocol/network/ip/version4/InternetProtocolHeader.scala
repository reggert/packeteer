package pakka.protocol.network.ip.version4

import scala.language.postfixOps
import pakka.protocol.network.ip
import pakka.util.Unsigned
import scala.annotation.tailrec
import scala.collection.immutable


final case class InternetProtocolHeader(
		trafficClass : ip.TrafficClass,
		totalLength : Int,
		identification : Identification,
		flags : Flags,
		fragmentOffset : FragmentOffset, 
		timeToLive : ip.HopCount,
		nextProtocol : ip.HeaderType,
		headerChecksum : HeaderChecksum,
		source : InternetAddress,
		destination : InternetAddress,
		options : List[HeaderOption]
	) extends ip.InternetProtocolHeader[InternetAddress]
{
	def hopLimit = timeToLive 
	def version = ip.ProtocolVersion(4)
	
	@tailrec
	private[this] def pad(n : Int) : Int = if (n % 4 == 0) n else pad(n+1) 
	
	private[this] def optionsLength = 
		options.view map {_.optionLength ensuring {_ >= 1}} sum
	
	val headerLength = InternetHeaderLength((5 + pad(optionsLength) / 4).toByte)
	
	require (totalLength >= headerLength.bytes)
	require (totalLength < (1 << 16))
}


final case class InternetHeaderLength(toByte : Byte) extends AnyVal with Unsigned.ByteWrapper
{
	def words = toInt
	def bytes = words * 4
}


final case class Identification(toShort : Short) extends AnyVal with Unsigned.ShortWrapper


final case class Flags(reserved : Boolean, dontFragment : Boolean, moreFragments : Boolean) 


final case class FragmentOffset(toShort : Short) extends AnyVal with Unsigned.ShortWrapper

final case class HeaderChecksum(toShort : Short) extends AnyVal with Unsigned.ShortWrapper


trait HeaderOption
{
	def optionType : OptionType
	def optionLength : Int
	def optionData : immutable.IndexedSeq[Byte]
}


final case class OptionType(toByte : Byte) extends AnyVal
{
	def copied : Boolean = (toByte & 0x80) != 0
	def optionClass : Int = (toByte & 0x60) >> 5
	def optionNumber : Int = (toByte & 0x1f)
}



