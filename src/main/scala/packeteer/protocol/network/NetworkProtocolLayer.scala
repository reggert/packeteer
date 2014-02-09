package packeteer.protocol.network

import packeteer.protocol.EncapsulatingProtocolLayer

trait NetworkProtocolLayer[+HeaderType, +TrailerType] 
	extends EncapsulatingProtocolLayer[HeaderType, TrailerType]
