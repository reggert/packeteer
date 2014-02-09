package packeteer.protocol.network.ip.version4

import packeteer.protocol.network.ip
import java.net.Inet4Address
import scala.annotation.switch
import packeteer.protocol.network.ip.AddressFormatException

final case class InternetAddress(val octet1 : Byte, val octet2 : Byte, val octet3 : Byte, val octet4 : Byte) 
	extends ip.InternetAddress with Ordered[InternetAddress]
{
	@throws(classOf[IndexOutOfBoundsException])
	override def apply(index : Int) = (index : @switch) match {
		case 0 => octet1
		case 1 => octet2
		case 2 => octet3
		case 3 => octet4
		case _ => throw new IndexOutOfBoundsException(index.toString)
	}
	
	override def toString = s"${octet1 & 0xff}.${octet2 & 0xff}.${octet3 & 0xff}.${octet4 & 0xff}"
	
	override def toInetAddress : Inet4Address = super.toInetAddress.asInstanceOf[Inet4Address]
	
	override def compare(other : InternetAddress) : Int =
		(this.octet1 & 0xff).compare(other.octet1 & 0xff) match
		{
			case 0 => (this.octet2 & 0xff).compare(other.octet2 & 0xff) match
			{
				case 0 => (this.octet3 & 0xff).compare(other.octet3 & 0xff) match
				{
					case 0 => (this.octet4 & 0xff).compare(other.octet4 & 0xff)
					case x => x
				}
				case x => x
			}
			case x => x
		}
}




object InternetAddress
{
	val Size = 4
	
	val DottedDecimalOctets = """(\d|[1-9]\d|1\d{2}|2[0-4]\d|25[0-5])\.(\d|[1-9]\d|1\d{2}|2[0-4]\d|25[0-5])\.(\d|[1-9]\d|1\d{2}|2[0-4]\d|25[0-5])\.(\d|[1-9]\d|1\d{2}|2[0-4]\d|25[0-5])""".r
	
	def apply(dottedDecimal : String) : InternetAddress = 
		parse(dottedDecimal) getOrElse {
			throw new AddressFormatException(s"Invalid IPv4 address: $dottedDecimal", dottedDecimal)
		}
		
	def parse(dottedDecimal : String) : Option[InternetAddress] = dottedDecimal match
	{
		case DottedDecimalOctets(octet1, octet2, octet3, octet4) =>
			// toInt needs to be called before toByte to avoid range checks
			Some(InternetAddress(octet1.toInt.toByte, octet2.toInt.toByte, octet3.toInt.toByte, octet4.toInt.toByte))
		case _ => None
	}

	def apply(octets : Seq[Byte]) : InternetAddress = octets match
	{
		case Seq(octet1, octet2, octet3, octet4) => InternetAddress(octet1, octet2, octet3, octet4)
		case _ => throw new IllegalArgumentException("octets.size != 4")
	}
}