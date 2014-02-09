package packeteer.protocol.network.ip

import scala.collection.immutable
import scala.annotation.switch
import java.net.InetAddress
import java.net.Inet4Address
import scala.collection.SeqView


trait InternetAddress extends immutable.IndexedSeq[Byte] with Product
{
	def toInetAddress : InetAddress = InetAddress.getByAddress(toArray)
	
	override final def length = productArity
	
	final def toIntSeq = view.map(_.toInt & 0xff)
	
	final def toShortSeq = toIntSeq.map(_.toShort)
}



object InternetAddress {
	
	def apply(octets : Seq[Byte]) : InternetAddress = octets.size match
	{
		case version4.InternetAddress.Size => version4.InternetAddress(octets)
		case version6.InternetAddress.Size => version6.InternetAddress(octets)
		case _ => throw new IllegalArgumentException("octets.size = ${octets.size}")
	}
		
	
	def apply(s : String) : InternetAddress = parse(s) getOrElse {
		throw new AddressFormatException(s)
	}
	
	
	def parse(s : String) : Option[InternetAddress] = 
		version4.InternetAddress.parse(s) orElse version6.InternetAddress.parse(s)
}



