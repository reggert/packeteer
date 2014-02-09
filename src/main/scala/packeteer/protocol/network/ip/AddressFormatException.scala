package packeteer.protocol.network.ip

class AddressFormatException(message : String, val badAddress : String) extends Exception(message) 
{
	def this(badAddress : String) = this(s"Invalid IP address: $badAddress", badAddress)
}