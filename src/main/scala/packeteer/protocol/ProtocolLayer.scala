package packeteer.protocol

import akka.util.ByteString

trait ProtocolLayer[+HeaderType, +PayloadType, +TrailerType]
{
	def header : HeaderType
	def trailer : TrailerType
	def payload : PayloadType
}