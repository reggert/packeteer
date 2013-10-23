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
			case Input.EOF =>
				assert (buffer.size < pcap.FileHeader.Size)
				Error("insufficient input", Input.El(buffer))
			case Input.Empty => 
				assert (buffer.size < pcap.FileHeader.Size)
				Cont(step(buffer))
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
			case Input.EOF => 
				assert (buffer.size < pcap.PacketHeader.Size)
				Error("insufficient input", Input.El(buffer))
			case Input.Empty => 
				assert (buffer.size < pcap.PacketHeader.Size)
				Cont(step(buffer))
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
			case Input.EOF =>
				assert (buffer.size < packetHeader.capturedLength)
				Error("insufficient input", Input.El(buffer))
			case Input.Empty => 
				assert (buffer.size < packetHeader.capturedLength)
				Cont(step(buffer))
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
		
		
	def packetsParser[O]
		(fileHeader : pcap.FileHeader)
		(initialValue : O)
		(handlePacket : (pcap.Packet, O) => O)
		(implicit executionContext : ExecutionContext) 
		: Iteratee[ByteString, O] =
	Cont {
			case Input.EOF => Done(initialValue, Input.EOF)
			case Input.El(e) if e.length > 0 => 
				val parseNextPacket = for {
					packet <- Iteratee.flatten(packetParser(fileHeader) feed Input.El(e))
				} yield handlePacket(packet, initialValue)
				parseNextPacket flatMap {newValue => packetsParser(fileHeader)(newValue)(handlePacket)}
			case _ => packetsParser(fileHeader)(initialValue)(handlePacket)
		}
}