package pakka.file.pcap.iteratee

import scala.collection.IndexedSeqOptimized
import scala.collection.immutable
import scala.concurrent.ExecutionContext
import play.api.libs.iteratee.{Cont, Done, Error, Input, Iteratee}
import pakka.file.pcap
import play.api.libs.iteratee.Step

trait ParserIteratees[Bytes <: immutable.IndexedSeq[Byte] with IndexedSeqOptimized[Byte, Bytes]]
{
	def fileHeaderParser(implicit byteSeqOps : ByteSeqOps[Bytes]) : Iteratee[Bytes, pcap.FileHeader] = 
	{	
		def step(buffer : Bytes)(input : Input[Bytes]) : Iteratee[Bytes, pcap.FileHeader] = input match
		{
			case Input.EOF =>
				assert (buffer.size < pcap.FileHeader.Size)
				Error("insufficient input", Input.El(buffer))
			case Input.Empty => 
				assert (buffer.size < pcap.FileHeader.Size)
				Cont(step(buffer))
			case Input.El(moreBytes) => byteSeqOps.concat(buffer, moreBytes) match
			{
				case combinedInput if combinedInput.size < pcap.FileHeader.Size =>
					Cont(step(combinedInput))
				case combinedInput =>
					try {
						val (headerBytes, remainder) = combinedInput splitAt pcap.FileHeader.Size
						Done(pcap.FileHeader(headerBytes), Input.El(remainder))
					}
					catch {
						case e : pcap.InvalidFileHeaderException => 
							Error(e.getMessage, Input.El(combinedInput))
					}
			}
		}
		Cont(step(byteSeqOps.empty))
	}
	
	
	def frameHeaderParser(fileHeader : pcap.FileHeader) (implicit byteSeqOps : ByteSeqOps[Bytes]) 
		: Iteratee[Bytes, pcap.FrameHeader] =
	{
		implicit val byteOrder = fileHeader.byteOrder
		implicit val timestampPrecision = fileHeader.timestampPrecision
		
		def step(buffer : Bytes)(input : Input[Bytes])
			: Iteratee[Bytes, pcap.FrameHeader] = input match
		{
			case Input.EOF => 
				assert (buffer.size < pcap.FrameHeader.Size)
				Error("insufficient input", Input.El(buffer))
			case Input.Empty => 
				assert (buffer.size < pcap.FrameHeader.Size)
				Cont(step(buffer))
			case Input.El(moreBytes) => byteSeqOps.concat(buffer, moreBytes) match
			{
				case combinedInput if combinedInput.size < pcap.FrameHeader.Size =>
					Cont(step(combinedInput))
				case combinedInput =>
				try {
					val (headerBytes, remainder) = combinedInput splitAt pcap.FrameHeader.Size
					Done(pcap.FrameHeader(headerBytes, fileHeader.snapshotLength), Input.El(remainder))
				}
				catch {
					case e : pcap.InvalidFrameHeaderException => Error(e.getMessage, Input.El(combinedInput))
				}
			}
		}
		
		Cont(step(byteSeqOps.empty))
	}
	
	
	def framePayloadParser(frameHeader : pcap.FrameHeader) (implicit byteSeqOps : ByteSeqOps[Bytes])  
		: Iteratee[Bytes, Bytes] =
	{
		def step(buffer : Bytes)(input : Input[Bytes])
			: Iteratee[Bytes, Bytes] = input match
		{
			case Input.EOF =>
				assert (buffer.size < frameHeader.capturedLength)
				Error("insufficient input", Input.El(buffer))
			case Input.Empty => 
				assert (buffer.size < frameHeader.capturedLength)
				Cont(step(buffer))
			case Input.El(moreBytes) => byteSeqOps.concat(buffer, moreBytes) match
			{
				case combinedInput if combinedInput.size < frameHeader.capturedLength =>
					Cont(step(combinedInput))
				case combinedInput =>
					val (payload, remainder) = combinedInput splitAt frameHeader.capturedLength
					Done(payload, Input.El(remainder))
			}
		}
		
		Cont(step(byteSeqOps.empty))
	}
	
	
	def frameParser(fileHeader : pcap.FileHeader)
		(implicit executionContext : ExecutionContext, byteSeqOps : ByteSeqOps[Bytes]) 
		: Iteratee[Bytes, pcap.Frame] =
		for {
			frameHeader <- frameHeaderParser(fileHeader)
			payload <- framePayloadParser(frameHeader)
		} yield pcap.Frame(fileHeader, frameHeader, payload)
		
		
	def framesParser[O]
		(fileHeader : pcap.FileHeader)
		(consumeFrame : Iteratee[pcap.Frame, O])
		(implicit executionContext : ExecutionContext, byteSeqOps : ByteSeqOps[Bytes]) 
		: Iteratee[Bytes, O] =
	Cont {
			case Input.EOF =>
				Iteratee.flatten(consumeFrame feed Input.EOF) pureFlatFold {
					case Step.Done(result, _) => Done(result, Input.EOF)
					case Step.Error(message, _) => Error(message, Input.EOF)
					case Step.Cont(_) => Error("Frame consumer failed to terminate", Input.EOF)
				}
			case Input.El(e) if e.length > 0 => 
				val parseNextFrame = for {
					packet <- Iteratee.flatten(frameParser(fileHeader) feed Input.El(e))
				} yield Iteratee.flatten(consumeFrame feed Input.El(packet))
				parseNextFrame flatMap {(consumeNextFrame => framesParser(fileHeader)(consumeNextFrame))}
			case _ => framesParser(fileHeader)(consumeFrame)
		}
		
	
}