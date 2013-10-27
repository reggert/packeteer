package pakka.protocol.network

import pakka.protocol.EncapsulatingProtocolLayer

trait NetworkProtocolLayer[+HeaderType, +TrailerType] 
	extends EncapsulatingProtocolLayer[HeaderType, TrailerType]
