package pakka.protocol

import akka.util.ByteString

trait EncapsulatingProtocolLayer[+HeaderType, +TrailerType] extends ProtocolLayer[HeaderType, ByteString, TrailerType] 
