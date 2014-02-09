package packeteer.file.pcap

import org.threeten.bp.Instant
import java.nio.ByteOrder
import scala.collection.immutable
import FrameHeader._
import scala.collection.IndexedSeqOptimized
import scala.annotation._

/**
 * PCAP packet header format, as described by http://wiki.wireshark.org/Development/LibpcapFileFormat
 */
case class FrameHeader(timestamp : Instant, capturedLength : Int, originalLength : Int) 
{
	sealed private abstract class BaseIndexedSeq(implicit timestampPrecision : FileHeader.TimestampPrecision) 
		extends immutable.IndexedSeq[Byte] 
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
			case 0 => firstByte(timestamp.getEpochSecond().toInt)
			case 1 => secondByte(timestamp.getEpochSecond().toInt)
			case 2 => thirdByte(timestamp.getEpochSecond().toInt)
			case 3 => fourthByte(timestamp.getEpochSecond().toInt)
			case 4 => firstByte(subsecondPortion(timestamp))
			case 5 => secondByte(subsecondPortion(timestamp))
			case 6 => thirdByte(subsecondPortion(timestamp))
			case 7 => fourthByte(subsecondPortion(timestamp))
			case 8 => firstByte(capturedLength)
			case 9 => secondByte(capturedLength)
			case 10 => thirdByte(capturedLength)
			case 11 => fourthByte(capturedLength)
			case 12 => firstByte(originalLength)
			case 13 => secondByte(originalLength)
			case 14 => thirdByte(originalLength)
			case 15 => fourthByte(originalLength)
			case _ => throw new IndexOutOfBoundsException(s"$index not in [0, 16)")
		}
	}
	
	
	def toIndexedSeq(implicit byteOrder : ByteOrder, timestampPrecision : FileHeader.TimestampPrecision) 
		: immutable.IndexedSeq[Byte] = byteOrder match
	{
		case ByteOrder.BIG_ENDIAN => new BigEndianIndexedSeq
		case ByteOrder.LITTLE_ENDIAN => new LittleEndianIndexedSeq
	}
	
	
	private final class BigEndianIndexedSeq(implicit timestampPrecision : FileHeader.TimestampPrecision)  
		extends BaseIndexedSeq
	{
		protected override def firstByte(x : Int) : Byte = mostSignificantByte(x)
		protected override def secondByte(x : Int) : Byte = secondMostSignificantByte(x)
		protected override def thirdByte(x : Int) : Byte = secondLeastSignificantByte(x)
		protected override def fourthByte(x : Int) : Byte = leastSignificantByte(x)
		protected override def firstByte(x : Short) : Byte = mostSignificantByte(x)
		protected override def secondByte(x : Short) : Byte = leastSignificantByte(x)
	}
	
	
	private final class LittleEndianIndexedSeq(implicit timestampPrecision : FileHeader.TimestampPrecision) 
		extends BaseIndexedSeq
	{
		protected override def firstByte(x : Int) : Byte = leastSignificantByte(x)
		protected override def secondByte(x : Int) : Byte = secondLeastSignificantByte(x)
		protected override def thirdByte(x : Int) : Byte = secondMostSignificantByte(x)
		protected override def fourthByte(x : Int) : Byte = mostSignificantByte(x)
		protected override def firstByte(x : Short) : Byte = leastSignificantByte(x)
		protected override def secondByte(x : Short) : Byte = mostSignificantByte(x)
	}
}


object FrameHeader
{
	val Size = 16
	
	
	def apply(bytes : IndexedSeq[Byte], snapshotLength : Int) 
		(implicit byteOrder : ByteOrder, timestampPrecision : FileHeader.TimestampPrecision)
		: FrameHeader =
	{
		require(bytes.length == Size)
		require(snapshotLength > 0)
		val (extractInt, extractShort) = byteOrder match
		{
			case ByteOrder.BIG_ENDIAN => (bigEndianInt(bytes), bigEndianShort(bytes))
			case ByteOrder.LITTLE_ENDIAN => (littleEndianInt(bytes), littleEndianShort(bytes))
		}
		val timestampSeconds = extractInt(0) & 0xffffffffL
		val timestampSubseconds = extractInt(4) & 0xffffffffL
		val timestamp = timestampPrecision match
		{
			case FileHeader.TimestampPrecision.Microsecond =>
				Instant.ofEpochSecond(timestampSeconds, timestampSubseconds * 1000L)
			case FileHeader.TimestampPrecision.Nanosecond =>
				Instant.ofEpochSecond(timestampSeconds, timestampSubseconds)
		}
		val capturedLength = extractInt(8) match {
			case x if x < 0 || x > snapshotLength => 
				throw new InvalidFrameHeaderException(s"captured length $x exceeds snapshot length $snapshotLength")
			case x => x
		}
		val originalLength = extractInt(12) match {
			case x if x < 0 =>
				throw new InvalidFrameHeaderException(s"unreasonable original frame length ${x & 0xffffffffL}")
			case x => x
		}
		FrameHeader(timestamp, capturedLength, originalLength)
	}
	
	
	private def subsecondPortion(timestamp : Instant)(implicit timestampPrecision : FileHeader.TimestampPrecision) =
		timestampPrecision match
		{
			case FileHeader.TimestampPrecision.Microsecond =>
				timestamp.getNano / 1000
			case FileHeader.TimestampPrecision.Nanosecond =>
				timestamp.getNano
		}
}


