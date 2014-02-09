package packeteer.protocol.network.ip

import packeteer.protocol.network.NetworkProtocolLayer

trait InternetProtocolLayer[+HeaderType <: InternetProtocolHeader[_]] 
	extends NetworkProtocolLayer[HeaderType, Nothing]

