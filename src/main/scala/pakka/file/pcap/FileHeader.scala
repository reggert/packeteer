package pakka.file.pcap

import java.nio.ByteOrder
import pakka.util.Unsigned

trait FileHeader 
{
	def magicNumber : MagicNumber
	def versionMajor : VersionNumber
	def versionMinor : VersionNumber
	def timeZone : TimeZoneOffset
	def sigfigs : SignificantFigures
	def byteOrder : ByteOrder
	def linkType : LinkType
}



final case class MagicNumber(toInt : Int) extends AnyVal with Unsigned.IntWrapper

final case class VersionNumber(toShort : Short) extends AnyVal with Unsigned.ShortWrapper

final case class TimeZoneOffset(toInt : Int) extends AnyVal

final case class SignificantFigures(toInt : Int) extends AnyVal with Unsigned.IntWrapper

