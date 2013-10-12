package pakka.file.pcap

import org.threeten.bp.Instant

/**
 * PCAP packet header format, as described by http://wiki.wireshark.org/Development/LibpcapFileFormat
 */
trait PacketHeader 
{
	def timestamp : Instant
	def capturedLength : Int
	def originalLength : Int
}


