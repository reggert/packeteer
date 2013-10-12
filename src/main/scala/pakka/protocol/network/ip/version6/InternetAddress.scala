package pakka.protocol.network.ip.version6

import pakka.protocol.network.ip
import java.net.Inet6Address
import scala.annotation.switch

final case class InternetAddress(
		val octet1 : Byte,
		val octet2 : Byte,
		val octet3 : Byte,
		val octet4 : Byte,
		val octet5 : Byte,
		val octet6 : Byte,
		val octet7 : Byte,
		val octet8 : Byte,
		val octet9 : Byte,
		val octet10 : Byte,
		val octet11 : Byte,
		val octet12 : Byte,
		val octet13 : Byte,
		val octet14 : Byte,
		val octet15 : Byte,
		val octet16 : Byte
	) extends ip.InternetAddress with Ordered[InternetAddress]
{
	@throws(classOf[IndexOutOfBoundsException])
	override def apply(index : Int) = (index : @switch) match {
		case 0 => octet1
		case 1 => octet2
		case 2 => octet3
		case 3 => octet4
		case 4 => octet5
		case 5 => octet6
		case 6 => octet7
		case 7 => octet8
		case 8 => octet9
		case 9 => octet10
		case 10 => octet11
		case 11 => octet12
		case 12 => octet13
		case 13 => octet14
		case 14 => octet15
		case 15 => octet16
		case _ => throw new IndexOutOfBoundsException(index.toString)
	}
	
	override def toString =
		f"${octet1}%02X${octet2}%02X:${octet3}%02X${octet4}%02X:${octet5}%02X${octet6}%02X:${octet7}%02X${octet8}%02X:${octet9}%02X${octet10}%02X:${octet11}%02X${octet12}%02X:${octet13}%02X${octet14}%02X:${octet15}%02X${octet16}%02X"
		
	override def compare(other : InternetAddress) : Int =
		this.toIntSeq.zip(other.toIntSeq) map {case (x, y) => x - y} find {_ != 0} getOrElse 0
}



object InternetAddress 
{
	object ColonGroups
	{
		def apply(address : InternetAddress) = address.toString
	}
	
	object OctetSequence
	{
		def apply(address : InternetAddress) : Seq[Byte] = address
		
		def unapply(octets : Seq[Byte]) : Option[InternetAddress] = octets match
		{
			case Seq(
					octet1, octet2, octet3, octet4, octet5, octet6, octet7, octet8,
					octet9, octet10, octet11, octet12, octet13, octet14, octet15, octet16
				) => 
				Some(InternetAddress(
						octet1, octet2, octet3, octet4, octet5, octet6, octet7, octet8,
						octet9, octet10, octet11, octet12, octet13, octet14, octet15, octet16
					))
			case _ => None
		}
	}
}

