package pakka.protocol.network.ip

import scala.collection.immutable
import scala.annotation.switch
import java.net.InetAddress
import java.net.Inet4Address
import scala.collection.SeqView

sealed trait InternetAddress extends immutable.IndexedSeq[Byte] with Product
{
	def toInetAddress : InetAddress = InetAddress.getByAddress(toArray)
	
	override final def length = productArity
	
	final def toIntSeq = view.map(_.toInt & 0xff)
	
	final def toShortSeq = toIntSeq.map(_.toShort)
}


final case class InternetAddressVersion4(val octet1 : Byte, val octet2 : Byte, val octet3 : Byte, val octet4 : Byte) 
	extends InternetAddress with Ordered[InternetAddressVersion4]
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
	
	override def compare(other : InternetAddressVersion4) : Int =
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



final case class InternetAddressVersion6(
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
	) extends InternetAddress with Ordered[InternetAddressVersion6]
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
		
	override def compare(other : InternetAddressVersion6) : Int =
		this.toIntSeq.zip(other.toIntSeq) map {case (x, y) => x - y} find {_ != 0} getOrElse 0
}


object InternetAddress {
	
	object OctetSequence
	{
		def apply(address : InternetAddress) : Seq[Byte] = address
		
		def unapply(octets : Seq[Byte]) : Option[InternetAddress] = octets match
		{
			case InternetAddressVersion4.OctetSequence(address) => Some(address)
			case InternetAddressVersion6.OctetSequence(address) => Some(address)
			case _ => None
		}
	}
}


object InternetAddressVersion4 
{
	object DottedDecimal 
	{
		val Pattern = """(\d|[1-9]\d|1\d{2}|2[0-4]\d|25[0-5])\.(\d|[1-9]\d|1\d{2}|2[0-4]\d|25[0-5])\.(\d|[1-9]\d|1\d{2}|2[0-4]\d|25[0-5])\.(\d|[1-9]\d|1\d{2}|2[0-4]\d|25[0-5])""".r
		
		def apply(address : InternetAddressVersion4) = address.toString
		
		def unapply(s : String) : Option[InternetAddressVersion4] = s match
		{
			case Pattern(octet1, octet2, octet3, octet4) => 
				// toInt needs to be called before toByte to avoid range checks
				Some(InternetAddressVersion4(octet1.toInt.toByte, octet2.toInt.toByte, octet3.toInt.toByte, octet4.toInt.toByte))
			case _ =>
				None
		}
	}
	
	object OctetSequence
	{
		def apply(address : InternetAddressVersion4) : Seq[Byte] = address
	
		def unapply(octets : Seq[Byte]) : Option[InternetAddressVersion4] = octets match
		{
			case Seq(octet1, octet2, octet3, octet4) => Some(InternetAddressVersion4(octet1, octet2, octet3, octet4))
			case _ => None
		}
	}
}


object InternetAddressVersion6 
{
	object ColonGroups
	{
		def apply(address : InternetAddressVersion6) = address.toString
	}
	
	object OctetSequence
	{
		def apply(address : InternetAddressVersion6) : Seq[Byte] = address
		
		def unapply(octets : Seq[Byte]) : Option[InternetAddressVersion6] = octets match
		{
			case Seq(
					octet1, octet2, octet3, octet4, octet5, octet6, octet7, octet8,
					octet9, octet10, octet11, octet12, octet13, octet14, octet15, octet16
				) => 
				Some(InternetAddressVersion6(
						octet1, octet2, octet3, octet4, octet5, octet6, octet7, octet8,
						octet9, octet10, octet11, octet12, octet13, octet14, octet15, octet16
					))
			case _ => None
		}
	}
}
