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
	
	
	def frameHeaderParser(fileHeader : pcap.FileHeader) : Iteratee[ByteString, pcap.FrameHeader] =
	{
		implicit val byteOrder = fileHeader.byteOrder
		implicit val timestampPrecision = fileHeader.timestampPrecision
		
		def step(buffer : ByteString)(input : Input[ByteString])
			: Iteratee[ByteString, pcap.FrameHeader] = input match
		{
			case Input.EOF => 
				assert (buffer.size < pcap.FrameHeader.Size)
				Error("insufficient input", Input.El(buffer))
			case Input.Empty => 
				assert (buffer.size < pcap.FrameHeader.Size)
				Cont(step(buffer))
			case Input.El(moreBytes) => (buffer ++ moreBytes) match
			{
				case combinedInput if combinedInput.size < pcap.FrameHeader.Size =>
					Cont(step(combinedInput))
				case combinedInput =>
				try {
					val (headerBytes, remainder) = combinedInput splitAt pcap.FrameHeader.Size
					Done(pcap.FrameHeader(headerBytes, fileHeader.snapshotLength), Input.El(remainder))
				}
				catch {
					case e : InvalidFrameHeaderException => Error(e.getMessage, Input.El(combinedInput))
				}
			}
		}
		
		Cont(step(ByteString.empty))
	}
	
	
	def framePayloadParser(frameHeader : pcap.FrameHeader) : Iteratee[ByteString, ByteString] =
	{
		def step(buffer : ByteString)(input : Input[ByteString])
			: Iteratee[ByteString, ByteString] = input match
		{
			case Input.EOF =>
				assert (buffer.size < frameHeader.capturedLength)
				Error("insufficient input", Input.El(buffer))
			case Input.Empty => 
				assert (buffer.size < frameHeader.capturedLength)
				Cont(step(buffer))
			case Input.El(moreBytes) => (buffer ++ moreBytes) match
			{
				case combinedInput if combinedInput.size < frameHeader.capturedLength =>
					Cont(step(combinedInput))
				case combinedInput =>
					val (payload, remainder) = combinedInput splitAt frameHeader.capturedLength
					Done(payload, Input.El(remainder))
			}
		}
		
		Cont(step(ByteString.empty))
	}
	
	
	def frameParser(fileHeader : pcap.FileHeader)(implicit executionContext : ExecutionContext) : Iteratee[ByteString, pcap.Frame] =
		for {
			frameHeader <- frameHeaderParser(fileHeader)
			payload <- framePayloadParser(frameHeader)
		} yield pcap.Frame(fileHeader, frameHeader, payload)
		
		
	def framesParser[O]
		(fileHeader : pcap.FileHeader)
		(initialValue : O)
		(handleFrame : (pcap.Frame, O) => O)
		(implicit executionContext : ExecutionContext) 
		: Iteratee[ByteString, O] =
	Cont {
			case Input.EOF => Done(initialValue, Input.EOF)
			case Input.El(e) if e.length > 0 => 
				val parseNextFrame = for {
					packet <- Iteratee.flatten(frameParser(fileHeader) feed Input.El(e))
				} yield handleFrame(packet, initialValue)
				parseNextFrame flatMap {(newValue => framesParser(fileHeader)(newValue)(handleFrame))}
			case _ => framesParser(fileHeader)(initialValue)(handleFrame)
		}
}