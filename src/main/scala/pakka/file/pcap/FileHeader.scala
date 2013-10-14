package pakka.file.pcap

import java.nio.ByteOrder
import pakka.util.Unsigned
import scala.collection.immutable
import FileHeader._

trait FileHeader 
{
	def byteOrder : ByteOrder 
	def magicNumber : MagicNumber
	def versionMajor : VersionNumber
	def versionMinor : VersionNumber
	def timeZone : TimeZoneOffset
	def sigFigs : SignificantFigures
	def snapshotLength : Int
	def linkType : LinkType
}



final case class MagicNumber(toInt : Int) extends AnyVal with Unsigned.IntWrapper
{
	def swapped = MagicNumber(
			((toInt >> 24) & 0x000000ff) |
			((toInt >> 8) & 0x0000ff00) |
			((toInt << 8) & 0x00ff0000) |
			((toInt << 24) & 0xff000000)
		)
}

final case class VersionNumber(toShort : Short) extends AnyVal with Unsigned.ShortWrapper

final case class TimeZoneOffset(toInt : Int) extends AnyVal

final case class SignificantFigures(toInt : Int) extends AnyVal with Unsigned.IntWrapper



object FileHeader
{
	val Size = 24
	
	object MagicNumbers
	{
		val Standard = MagicNumber(0xa1b2c3d4)
		val NanosecondPrecision = MagicNumber(0xa1b23c4d)
		val Modified = MagicNumber(0xa1b2cd34)
		
		val Supported = Set(Standard, NanosecondPrecision, Modified)
		val Swapped = Supported map {_.swapped}
	}
	
	
	def apply(fileHeader : FileHeader) : FileHeader = new PlainFileHeader(fileHeader)
	
	def apply(bytes : immutable.IndexedSeq[Byte]) : FileHeader = new SeqBackedFileHeader(bytes)
	
	def apply(
			byteOrder : ByteOrder, 
			magicNumber : MagicNumber,
			versionMajor : VersionNumber,
			versionMinor : VersionNumber,
			timeZone : TimeZoneOffset,
			sigFigs : SignificantFigures,
			snapshotLength : Int,
			linkType : LinkType
		) : FileHeader = PlainFileHeader(
				byteOrder, 
				magicNumber, 
				versionMajor, 
				versionMinor, 
				timeZone, 
				sigFigs,
				snapshotLength, 
				linkType
			)
}