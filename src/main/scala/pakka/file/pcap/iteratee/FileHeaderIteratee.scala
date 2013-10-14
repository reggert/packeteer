package pakka.file.pcap.iteratee

import akka.util.ByteString
import pakka.file.pcap
import play.api.libs.iteratee.{Input, Iteratee, Error}
import pakka.file.pcap.InvalidFileHeaderException
import pakka.file.pcap.FileHeader
import play.api.libs.iteratee.Done
import play.api.libs.iteratee.Cont

object FileHeaderIteratee 
{
	def step(buffer : ByteString)(input : Input[ByteString]) : Iteratee[ByteString, pcap.FileHeader] = input match
	{
		case Input.EOF => Error("insufficient input", Input.El(buffer))
		case Input.Empty => Cont(step(buffer))
		case Input.El(moreBytes) => 
			val combinedInput = buffer ++ moreBytes
			if (combinedInput.size >= pcap.FileHeader.Size)
				try {
					val (headerBytes, remainder) = combinedInput splitAt pcap.FileHeader.Size
					val fileHeader = FileHeader(headerBytes)
					Done(fileHeader, Input.El(remainder))
				}
				catch {
					case e : InvalidFileHeaderException => 
						Error(e.getMessage, Input.El(combinedInput))
				}
			else
				Cont(step(combinedInput))
	}
	
	
	def apply() : Iteratee[ByteString, pcap.FileHeader] = Cont(step(ByteString.empty))
}