package pakka.file.pcap.iteratee

import java.util.concurrent.ForkJoinPool
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scala.language.postfixOps
import org.scalatest.{FeatureSpec, FlatSpec, GivenWhenThen, Matchers, WordSpec}
import akka.util.ByteString
import pakka.file.pcap
import play.api.libs.iteratee.{Enumerator, Iteratee}
import java.nio.ByteOrder
import org.threeten.bp.Instant

class ParserIterateeTest extends FeatureSpec with Matchers with GivenWhenThen
	with ParserIteratees[ByteString] with ByteSeqOps.Implicits
{
	val executor = new ForkJoinPool
	implicit val executionContext = ExecutionContext.fromExecutorService(executor)
	
	// Source of capture file: http://wiki.wireshark.org/SampleCaptures?action=AttachFile&do=get&target=smtp.pcap
	private def smtpPcapStream = getClass.getResourceAsStream("/smtp.pcap") 
	
	private def smtpPcapEnumerator(chunkSize : Int = 10, skip : Int = 0) =
	{
		val stream = smtpPcapStream
		stream.skip(skip)
		Enumerator.fromStream(stream, chunkSize) map (ByteString(_))
	}
	
	feature("The PCAP file parsing iteratees") {
		
		val expectedFileHeader = pcap.FileHeader(
				byteOrder = ByteOrder.LITTLE_ENDIAN,
				magicNumber = pcap.FileHeader.MagicNumbers.Standard,
				version = pcap.Version(
						pcap.VersionNumber(2),
						pcap.VersionNumber(4)
					),
				timeZone = pcap.TimeZoneOffset(0),
				sigFigs = pcap.SignificantFigures(0),
				snapshotLength = 65535,
				linkType = pcap.LinkTypes.LINKTYPE_ETHERNET
			)

		scenario("PCAP file header is parsed") {
			Given("a PCAP file loaded from an InputStream")
			When("the file is pushed into the fileHeaderParser by an Enumerator")
			val resultFuture = smtpPcapEnumerator().run(fileHeaderParser)
			
			Then("it should parse the file header")
			val result : pcap.FileHeader = Await.result(resultFuture, atMost = 2 seconds)

			Then("the parsed header should contain the correct values")
			result should be (expectedFileHeader)
		}
		
		val expectedFrameHeader = pcap.FrameHeader(
				timestamp = Instant.parse("2009-10-05T06:06:07.492060Z"),
				capturedLength = 76,
				originalLength = 76
			)
		
		scenario("PCAP frame header is parsed") {
			Given("a PCAP file loaded from an InputStream, and the extracted file header")
			val enumerator = smtpPcapEnumerator(skip = pcap.FileHeader.Size)
			
			When("the remaining stream is pushed into the frameHeaderParser by an Enumerator")
			val resultFuture = enumerator.run(frameHeaderParser(expectedFileHeader))
			
			Then("it should parse the frame header")
			val result : pcap.FrameHeader = Await.result(resultFuture, atMost = 2 seconds)
			
			Then("the parsed header should contain the correct values")
			result should be (expectedFrameHeader)
		}
		
		scenario("PCAP frame payload is parsed") {
			Given("a PCAP file loaded from an InputStream, and the extracted file header and first frame header")
			val enumerator = smtpPcapEnumerator(skip = pcap.FileHeader.Size + pcap.FrameHeader.Size)
			
			When("the remaining stream is pushed into the framePayloadParser by an Enumerator")
			val resultFuture = enumerator.run(framePayloadParser(expectedFrameHeader))
			
			Then("it should extract the frame payload")
			val result : ByteString = Await.result(resultFuture, atMost = 2 seconds)
			
			Then("the extracted frame should be the correct length")
			result.length should be (expectedFrameHeader.capturedLength)
		}
		
		scenario("Entire PCAP frame is parsed") {
			Given("a PCAP file loaded from an InputStream, and the extracted file header")
			val enumerator = smtpPcapEnumerator(skip = pcap.FileHeader.Size)
			
			When("the remaining stream is pushed into the frameParser by an Enumerator")
			val resultFuture = enumerator.run(frameParser(expectedFileHeader))
			
			Then("it should parse the frame header and extract the frame payload")
			val result : pcap.Frame = Await.result(resultFuture, atMost = 2 seconds)
			
			Then("the parsed header should contain the correct values")
			result.frameHeader should be (expectedFrameHeader)
			
			Then("the extracted frame should be the correct length")
			result.payload.length should be (expectedFrameHeader.capturedLength)
		}
		
		scenario("All frames are parsed") {
			Given("a PCAP file loaded from an InputStream, and the extracted file header")
			val enumerator = smtpPcapEnumerator(skip = pcap.FileHeader.Size)
			
			When("the remaining stream is pushed into the frameParser by an Enumerator")
			val resultFuture = enumerator.run(
					framesParser(expectedFileHeader)(Iteratee.getChunks)
				)
			
			Then("it should parse all the frames")
			val result : List[pcap.Frame] = Await.result(resultFuture, atMost = 2 seconds)
			result.size should be (60)
		}
		
		scenario("Entire PCAP file is parsed") {
			Given("a PCAP file loaded from an InputStream")
			And("a parser iteratee formed by composing the fileHeaderParser with the framesParser")
			val parser = for {
				fileHeader <- fileHeaderParser
				count <- framesParser(fileHeader)(Iteratee.fold(0)((count, _) => count + 1))
			} yield count
			
			When("the file is pushed into the parser by an Enumerator")
			val resultFuture = smtpPcapEnumerator().run(parser)
			
			Then("it should parse the file header and all frames")
			val result : Int = Await.result(resultFuture, atMost = 2 seconds)
			result should be (60)
		}
		
	}
}