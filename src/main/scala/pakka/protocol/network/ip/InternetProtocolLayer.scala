package pakka.protocol.network.ip

import pakka.protocol.network.NetworkProtocolLayer

trait InternetProtocolLayer[+HeaderType <: InternetProtocolHeader[_]] 
	extends NetworkProtocolLayer[HeaderType, Nothing]

