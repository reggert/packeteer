package pakka.util

object Unsigned 
{
	trait ByteWrapper extends Any
	{
		def toByte : Byte
		def toInt = toByte & 0xff
		def toLong = toByte & 0xffL
	}
	
	trait ShortWrapper extends Any
	{
		def toShort : Short
		def toInt = toShort & 0xffff
		def toLong = toShort & 0xffffL
	}
	
	trait IntWrapper extends Any
	{
		def toInt : Int
		def toLong = toInt & 0xffffffffL
	}
}