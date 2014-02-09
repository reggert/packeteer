package packeteer.file.pcap

import java.nio.ByteOrder
import scala.collection.immutable
import packeteer.util._
import FileHeader._
import scala.collection.IndexedSeqOptimized
import scala.annotation._

case class FileHeader(
		byteOrder : ByteOrder, 
		magicNumber : MagicNumber,
		version : Version,
		timeZone : TimeZoneOffset,
		sigFigs : SignificantFigures,
		snapshotLength : Int,
		linkType : LinkType
	) 
{
	def timestampPrecision : TimestampPrecision = magicNumber match
	{
		case MagicNumbers.NanosecondPrecision => TimestampPrecision.Nanosecond
		case _ => TimestampPrecision.Microsecond
	}
	
	
	def toIndexedSeq : immutable.IndexedSeq[Byte] = byteOrder match
	{
		case ByteOrder.BIG_ENDIAN => new BigEndianIndexedSeq
		case ByteOrder.LITTLE_ENDIAN => new LittleEndianIndexedSeq
	}
	
	
	sealed private abstract class BaseIndexedSeq extends immutable.IndexedSeq[Byte] 
		with IndexedSeqOptimized[Byte, immutable.IndexedSeq[Byte]]
	{
		override final def length = Size
		
		protected def firstByte(x : Int) : Byte
		protected def secondByte(x : Int) : Byte
		protected def thirdByte(x : Int) : Byte
		protected def fourthByte(x : Int) : Byte
		protected def firstByte(x : Short) : Byte
		protected def secondByte(x : Short) : Byte
		
		override final def apply(index : Int) : Byte = (index : @switch) match
		{
			case 0 => firstByte(magicNumber.toInt)
			case 1 => secondByte(magicNumber.toInt)
			case 2 => thirdByte(magicNumber.toInt)
			case 3 => fourthByte(magicNumber.toInt)
			case 4 => firstByte(version.major.toShort)
			case 5 => secondByte(version.major.toShort)
			case 6 => firstByte(version.minor.toShort)
			case 7 => secondByte(version.minor.toShort)
			case 8 => firstByte(timeZone.toInt)
			case 9 => secondByte(timeZone.toInt)
			case 10 => thirdByte(timeZone.toInt)
			case 11 => fourthByte(timeZone.toInt)
			case 12 => firstByte(sigFigs.toInt)
			case 13 => secondByte(sigFigs.toInt)
			case 14 => thirdByte(sigFigs.toInt)
			case 15 => fourthByte(sigFigs.toInt)
			case 16 => firstByte(snapshotLength)
			case 17 => secondByte(snapshotLength)
			case 18 => thirdByte(snapshotLength)
			case 19 => fourthByte(snapshotLength)
			case 20 => firstByte(linkType.toInt)
			case 21 => secondByte(linkType.toInt)
			case 22 => thirdByte(linkType.toInt)
			case 23 => fourthByte(linkType.toInt)
			case _ => throw new IndexOutOfBoundsException(s"$index not in [0, 24)")
		}
	}
	
	
	private final class BigEndianIndexedSeq extends BaseIndexedSeq
	{
		protected override def firstByte(x : Int) : Byte = mostSignificantByte(x)
		protected override def secondByte(x : Int) : Byte = secondMostSignificantByte(x)
		protected override def thirdByte(x : Int) : Byte = secondLeastSignificantByte(x)
		protected override def fourthByte(x : Int) : Byte = leastSignificantByte(x)
		protected override def firstByte(x : Short) : Byte = mostSignificantByte(x)
		protected override def secondByte(x : Short) : Byte = leastSignificantByte(x)
	}
	
	
	private final class LittleEndianIndexedSeq extends BaseIndexedSeq
	{
		protected override def firstByte(x : Int) : Byte = leastSignificantByte(x)
		protected override def secondByte(x : Int) : Byte = secondLeastSignificantByte(x)
		protected override def thirdByte(x : Int) : Byte = secondMostSignificantByte(x)
		protected override def fourthByte(x : Int) : Byte = mostSignificantByte(x)
		protected override def firstByte(x : Short) : Byte = leastSignificantByte(x)
		protected override def secondByte(x : Short) : Byte = mostSignificantByte(x)
	}
}


final case class MagicNumber(toInt : Int) extends AnyVal with Unsigned.IntWrapper
{
	def swapped = MagicNumber(
			((toInt >> 24) & 0x000000ff) |
			((toInt >> 8) & 0x0000ff00) |
			((toInt << 8) & 0x00ff0000) |
			((toInt << 24) & 0xff000000)
		)
		
	override def toString = f"${toLong}%8x"
}

final case class Version(major : VersionNumber, minor : VersionNumber)

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
	
	
		
	@throws(classOf[InvalidFileHeaderException])
	def apply(bytes : IndexedSeq[Byte]) : FileHeader = 
	{
		require(bytes.length == Size)
		val rawMagicNumber : MagicNumber = 
			MagicNumber(bytesToInt(bytes(0), bytes(1), bytes(2), bytes(3)))
		val (extractInt, extractShort, byteOrder, magicNumber) = 
			if (MagicNumbers.Supported(rawMagicNumber))
				(bigEndianInt(bytes), bigEndianShort(bytes), ByteOrder.BIG_ENDIAN, rawMagicNumber)
			else if (MagicNumbers.Swapped(rawMagicNumber))
				(littleEndianInt(bytes), littleEndianShort(bytes), ByteOrder.LITTLE_ENDIAN, rawMagicNumber.swapped)
			else
				throw new InvalidFileHeaderException(f"Invalid magic number: ${rawMagicNumber.toInt}%x")
		extractInt(16) match
		{
			case snapshotLength if snapshotLength <= 0 =>
				throw new InvalidFileHeaderException(s"Unreasonable snapshot length: ${snapshotLength & 0xffffffffL}")
			case snapshotLength =>
				FileHeader(
						byteOrder,
						magicNumber,
						Version(
								VersionNumber(extractShort(4)),
								VersionNumber(extractShort(6))
							),
						TimeZoneOffset(extractInt(8)),
						SignificantFigures(extractInt(12)),
						snapshotLength,
						LinkType(extractInt(20))
					)
		}
	}
		
	
	object TimestampPrecision extends Enumeration
	{
		val Microsecond, Nanosecond = Value
	}
	
	type TimestampPrecision = TimestampPrecision.Value
	
}