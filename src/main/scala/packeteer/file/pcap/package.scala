package packeteer.file

import packeteer.util._

package object pcap 
{
	@inline private[pcap] def mostSignificantByte(x : Int) = (x >> 24).toByte
	@inline private[pcap] def secondMostSignificantByte(x : Int) = (x >> 16).toByte
	@inline private[pcap] def secondLeastSignificantByte(x : Int) = (x >> 8).toByte
	@inline private[pcap] def leastSignificantByte(x : Int) = x.toByte
	@inline private[pcap] def mostSignificantByte(x : Short) = (x >> 8).toByte
	@inline private[pcap] def leastSignificantByte(x : Short) = x.toByte
	
	
	@inline private[pcap] def bigEndianInt(bytes : IndexedSeq[Byte]) = (startAt : Int) =>
		bytesToInt(bytes(0+startAt), bytes(1+startAt), bytes(2+startAt), bytes(3+startAt))
		
	@inline private[pcap] def littleEndianInt(bytes : IndexedSeq[Byte]) = (startAt : Int) =>
		bytesToInt(bytes(3+startAt), bytes(2+startAt), bytes(1+startAt), bytes(0+startAt))
		
	@inline private[pcap] def bigEndianShort(bytes : IndexedSeq[Byte]) = (startAt : Int) =>
		bytesToShort(bytes(0+startAt), bytes(1+startAt))
		
	@inline private[pcap] def littleEndianShort(bytes : IndexedSeq[Byte]) = (startAt : Int) =>
		bytesToShort(bytes(1+startAt), bytes(0+startAt))
}