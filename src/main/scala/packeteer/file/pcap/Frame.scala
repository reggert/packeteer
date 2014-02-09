package packeteer.file.pcap

import scala.collection.immutable

final case class Frame(fileHeader : FileHeader, frameHeader : FrameHeader, payload : immutable.IndexedSeq[Byte]) 
