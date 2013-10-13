package pakka.file.pcap

final case class Packet(fileHeader : FileHeader, packetHeader : PacketHeader, payload : Seq[Byte]) 
