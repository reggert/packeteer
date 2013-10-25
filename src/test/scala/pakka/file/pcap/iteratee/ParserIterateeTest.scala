package pakka.file.pcap.iteratee

import java.util.concurrent.ForkJoinPool
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scala.language.postfixOps
import org.scalatest.{FeatureSpec, FlatSpec, GivenWhenThen, Matchers, WordSpec}
import akka.util.ByteString
import pakka.file.pcap
import play.api.libs.iteratee.Enumerator
import java.nio.ByteOrder
import org.threeten.bp.Instant

class ParserIterateeTest extends FeatureSpec with Matchers with GivenWhenThen
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
		
		val expectedPacketHeader = pcap.PacketHeader(
				timestamp = Instant.parse("2009-10-05T06:06:07.492060Z"),
				capturedLength = 76,
				originalLength = 76
			)
		
		scenario("PCAP packet header is parsed") {
			Given("a PCAP file loaded from an InputStream, and the extracted file header")
			val enumerator = smtpPcapEnumerator(skip = pcap.FileHeader.Size)
			
			When("the remaining stream is pushed into the packetHeaderParser by an Enumerator")
			val resultFuture = enumerator.run(packetHeaderParser(expectedFileHeader))
			
			Then("it should parse the packet header")
			val result : pcap.PacketHeader = Await.result(resultFuture, atMost = 2 seconds)
			
			Then("the parsed header should contain the correct values")
			result should be (expectedPacketHeader)
		}
		
		scenario("PCAP packet payload is parsed") {
			Given("a PCAP file loaded from an InputStream, and the extracted file header and first packet header")
			val enumerator = smtpPcapEnumerator(skip = pcap.FileHeader.Size + pcap.PacketHeader.Size)
			
			When("the remaining stream is pushed into the packetPayloadParser by an Enumerator")
			val resultFuture = enumerator.run(packetPayloadParser(expectedPacketHeader))
			
			Then("it should extract the packet payload")
			val result : ByteString = Await.result(resultFuture, atMost = 2 seconds)
			
			Then("the extracted packet should be the correct length")
			result.length should be (expectedPacketHeader.capturedLength)
		}
		
		scenario("Entire PCAP packet is parsed") {
			Given("a PCAP file loaded from an InputStream, and the extracted file header")
			val enumerator = smtpPcapEnumerator(skip = pcap.FileHeader.Size)
			
			When("the remaining stream is pushed into the packetParser by an Enumerator")
			val resultFuture = enumerator.run(packetParser(expectedFileHeader))
			
			Then("it should parse the packet header and extract the packet payload")
			val result : pcap.Packet = Await.result(resultFuture, atMost = 2 seconds)
			
			Then("the parsed header should contain the correct values")
			result.packetHeader should be (expectedPacketHeader)
			
			Then("the extracted packet should be the correct length")
			result.payload.length should be (expectedPacketHeader.capturedLength)
		}
		
		scenario("All packets are parsed") {
			Given("a PCAP file loaded from an InputStream, and the extracted file header")
			val enumerator = smtpPcapEnumerator(skip = pcap.FileHeader.Size)
			
			When("the remaining stream is pushed into the packetParser by an Enumerator")
			val resultFuture = enumerator.run(
					packetsParser(expectedFileHeader)(List.empty[pcap.Packet]) {_ :: _}
				)
			
			Then("it should parse all the packets")
			val result : List[pcap.Packet] = Await.result(resultFuture, atMost = 2 seconds)
			result.size should be (60)
		}
		
		scenario("Entire PCAP file is parsed") {
			Given("a PCAP file loaded from an InputStream")
			And("a parser iteratee formed by composing the fileHeaderParser with the packetsParser")
			val parser = for {
				fileHeader <- fileHeaderParser
				count <- packetsParser(fileHeader)(0) {(_, oldCount) => oldCount + 1}
			} yield count
			
			When("the file is pushed into the parser by an Enumerator")
			val resultFuture = smtpPcapEnumerator().run(parser)
			
			Then("it should parse the file header and all packets")
			val result : Int = Await.result(resultFuture, atMost = 2 seconds)
			result should be (60)
		}
		
	}
}