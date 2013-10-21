package pakka.file.pcap

import scala.collection.immutable

final case class Packet(fileHeader : FileHeader, packetHeader : PacketHeader, payload : immutable.IndexedSeq[Byte]) 
