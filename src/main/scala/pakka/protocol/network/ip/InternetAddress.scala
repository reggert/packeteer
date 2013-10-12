package pakka.protocol.network.ip

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
	
	object OctetSequence
	{
		def apply(address : InternetAddress) : Seq[Byte] = address
		
		def unapply(octets : Seq[Byte]) : Option[InternetAddress] = octets match
		{
			case version4.InternetAddress.OctetSequence(address) => Some(address)
			case version6.InternetAddress.OctetSequence(address) => Some(address)
			case _ => None
		}
	}
}



