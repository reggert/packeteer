package pakka.protocol.network.ip

import scala.collection.immutable
import scala.annotation.switch
import java.net.InetAddress
import java.net.Inet4Address

sealed trait InternetAddress extends immutable.IndexedSeq[Byte] with Product
{
	def toInetAddress : InetAddress = InetAddress.getByAddress(toArray)
	
	override final def length = productArity
}


final case class InternetAddressVersion4(val octet1 : Byte, val octet2 : Byte, val octet3 : Byte, val octet4 : Byte) 
	extends InternetAddress 
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
}



final case class InetAddressVersion6(
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
	) extends InternetAddress 
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
}