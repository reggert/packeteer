package pakka.file.pcap.iteratee

import akka.util.ByteString
import pakka.file.pcap
import play.api.libs.iteratee.{Cont, Error, Input, Iteratee}
import pakka.file.pcap.InvalidPacketHeaderException
import play.api.libs.iteratee.Done
import pakka.file.pcap.PacketHeader

object PacketHeaderIteratee {

	def step(fileHeader : pcap.FileHeader)(buffer : ByteString)(input : Input[ByteString])
		: Iteratee[ByteString, pcap.PacketHeader] =
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
		step(buffer)(input)
	}
	
	def apply(fileHeader : pcap.FileHeader) : Iteratee[ByteString, pcap.PacketHeader] = 
		Cont(step(fileHeader)(ByteString.empty))
}