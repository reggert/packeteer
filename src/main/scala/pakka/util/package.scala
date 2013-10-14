package pakka

package object util 
{
	def bytesToInt(msb : Byte, smsb : Byte, slsb : Byte, lsb : Byte) =
		((msb & 0xff) << 24) |
		((smsb & 0xff) << 16) |
		((slsb & 0xff) << 8) |
		((lsb & 0xff))
	
	def bytesToShort(msb : Byte, lsb : Byte) : Short = 
		(((msb & 0xff) << 8) | ((lsb & 0xff))).toShort
}