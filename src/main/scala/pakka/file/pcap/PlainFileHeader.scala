package pakka.file.pcap

import java.nio.ByteOrder

final case class PlainFileHeader(
		byteOrder : ByteOrder, 
		magicNumber : MagicNumber,
		versionMajor : VersionNumber,
		versionMinor : VersionNumber,
		timeZone : TimeZoneOffset,
		sigFigs : SignificantFigures,
		snapshotLength : Int,
		linkType : LinkType
	) extends FileHeader 
{
	def this(other : FileHeader) = this(
			other.byteOrder, 
			other.magicNumber, 
			other.versionMajor, 
			other.versionMinor, 
			other.timeZone, 
			other.sigFigs, 
			other.snapshotLength, 
			other.linkType
		)
}