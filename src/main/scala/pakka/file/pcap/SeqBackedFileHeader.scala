package pakka.file.pcap

import scala.collection.immutable
import scala.annotation.switch
import java.nio.ByteOrder
import scala.math.Ordering.ByteOrdering
import pakka.util._

final class SeqBackedFileHeader(val bytes : immutable.IndexedSeq[Byte]) extends FileHeader 
{
	require (bytes.length == FileHeader.Size)
	
	private val rawMagicNumber : MagicNumber = 
		MagicNumber(bytesToInt(bytes(0), bytes(1), bytes(2), bytes(3)))
		
	override val byteOrder = 
		if (FileHeader.MagicNumbers.Supported(rawMagicNumber))
			ByteOrder.BIG_ENDIAN
		else if (FileHeader.MagicNumbers.Swapped(rawMagicNumber))
			ByteOrder.LITTLE_ENDIAN
		else
			throw new InvalidFileHeaderException(s"Invalid magic number: $rawMagicNumber")
		
	override def magicNumber : MagicNumber = (byteOrder : @switch) match
	{
		case ByteOrder.BIG_ENDIAN => rawMagicNumber
		case ByteOrder.LITTLE_ENDIAN => rawMagicNumber.swapped
	}
	
	private def extractInt(toExtract : IndexedSeq[Byte]) = (byteOrder : @switch) match
	{
		case ByteOrder.BIG_ENDIAN => 
			bytesToInt(toExtract(0), toExtract(1), toExtract(2), toExtract(3))
		case ByteOrder.LITTLE_ENDIAN => 
			bytesToInt(toExtract(3), toExtract(2), toExtract(1), toExtract(0))
	}
	
	private def extractShort(toExtract : IndexedSeq[Byte]) = (byteOrder : @switch) match
	{
		case ByteOrder.BIG_ENDIAN => 
			bytesToShort(toExtract(0), toExtract(1))
		case ByteOrder.LITTLE_ENDIAN => 
			bytesToShort(toExtract(1), toExtract(0))
	}
		
	
	override def versionMajor = VersionNumber(extractShort(bytes.slice(4, 6)))
	override def versionMinor = VersionNumber(extractShort(bytes.slice(6, 8)))
	override def timeZone = TimeZoneOffset(extractInt(bytes.slice(8, 12)))
	override def sigfigs = SignificantFigures(extractInt(bytes.slice(12, 16)))
	override def snapshotLength = extractInt(bytes.slice(16, 20))
	override def linkType = LinkType(extractInt(bytes.slice(20, 24)))
}