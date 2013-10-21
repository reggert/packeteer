package pakka.file.pcap

import pakka.file.pcap
import akka.util.ByteString
import play.api.libs.iteratee._
import scala.concurrent.ExecutionContext

package object iteratee 
{
	def fileHeaderParser : Iteratee[ByteString, pcap.FileHeader] = 
	{	
		def step(buffer : ByteString)(input : Input[ByteString]) : Iteratee[ByteString, pcap.FileHeader] = input match
		{
			case Input.EOF => Error("insufficient input", Input.El(buffer))
			case Input.Empty => Cont(step(buffer))
			case Input.El(moreBytes) => (buffer ++ moreBytes) match
			{
				case combinedInput if combinedInput.size < pcap.FileHeader.Size =>
					Cont(step(combinedInput))
				case combinedInput =>
					try {
						val (headerBytes, remainder) = combinedInput splitAt pcap.FileHeader.Size
						Done(FileHeader(headerBytes), Input.El(remainder))
					}
					catch {
						case e : InvalidFileHeaderException => 
							Error(e.getMessage, Input.El(combinedInput))
					}
			}
		}
		Cont(step(ByteString.empty))
	}
	
	
	def packetHeaderParser(fileHeader : pcap.FileHeader) : Iteratee[ByteString, pcap.PacketHeader] =
	{
		implicit val byteOrder = fileHeader.byteOrder
		implicit val timestampPrecision = fileHeader.timestampPrecision
		
		def step(buffer : ByteString)(input : Input[ByteString])
			: Iteratee[ByteString, pcap.PacketHeader] = input match
		{
			case Input.EOF => Error("insufficient input", Input.El(buffer))
			case Input.Empty => Cont(step(buffer))
			case Input.El(moreBytes) => (buffer ++ moreBytes) match
			{
				case combinedInput if combinedInput.size < pcap.PacketHeader.Size =>
					Cont(step(combinedInput))
				case combinedInput =>
				try {
					val (headerBytes, remainder) = combinedInput splitAt pcap.PacketHeader.Size
					Done(pcap.PacketHeader(headerBytes, fileHeader.snapshotLength), Input.El(remainder))
				}
				catch {
					case e : InvalidPacketHeaderException => Error(e.getMessage, Input.El(combinedInput))
				}
			}
		}
		
		Cont(step(ByteString.empty))
	}
	
	
	def packetPayloadParser(packetHeader : pcap.PacketHeader) : Iteratee[ByteString, ByteString] =
	{
		def step(buffer : ByteString)(input : Input[ByteString])
			: Iteratee[ByteString, ByteString] = input match
		{
			case Input.EOF => Error("insufficient input", Input.El(buffer))
			case Input.Empty => Cont(step(buffer))
			case Input.El(moreBytes) => (buffer ++ moreBytes) match
			{
				case combinedInput if combinedInput.size < packetHeader.capturedLength =>
					Cont(step(combinedInput))
				case combinedInput =>
					val (payload, remainder) = combinedInput splitAt packetHeader.capturedLength
					Done(payload, Input.El(remainder))
			}
		}
		
		Cont(step(ByteString.empty))
	}
	
	
	def packetParser(fileHeader : pcap.FileHeader)(implicit executionContext : ExecutionContext) : Iteratee[ByteString, pcap.Packet] =
		for {
			packetHeader <- packetHeaderParser(fileHeader)
			payload <- packetPayloadParser(packetHeader)
		} yield pcap.Packet(fileHeader, packetHeader, payload)
}