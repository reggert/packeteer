package pakka.util

object Unsigned 
{
	trait ByteWrapper extends Any
	{
		def toByte : Byte
		def toInt = toByte & 0xff
	}
	
	trait ShortWrapper extends Any
	{
		def toShort : Short
		def toInt = toShort & 0xffff
	}
}