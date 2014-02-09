package packeteer.file.pcap.iteratee

import scala.collection.immutable
import akka.util.ByteString

trait ByteSeqOps[T <: immutable.IndexedSeq[Byte]] 
{
	type SeqType = T
	def concat(a : SeqType, b : SeqType) : SeqType
	def empty : SeqType
}


object ByteSeqOps
{
	trait Implicits
	{
		implicit val byteSeqOpsForIndexedSeq = new ByteSeqOps[immutable.IndexedSeq[Byte]] {
			override def concat(a : SeqType, b : SeqType) = a ++ b
			override def empty = immutable.IndexedSeq.empty[Byte]
		}
		
	
		implicit val byteSeqOpsForByteString = new ByteSeqOps[ByteString] {
			override def concat(a : ByteString, b : ByteString) = a ++ b
			override def empty = ByteString.empty
		}
	}
}