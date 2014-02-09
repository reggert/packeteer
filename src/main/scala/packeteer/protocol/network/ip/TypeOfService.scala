package packeteer.protocol.network.ip

import packeteer.util.Unsigned


final case class TypeOfService(toByte : Byte) extends AnyVal with Unsigned.ByteWrapper
{
	import TypeOfService._
	
	def precedence = Precedence(toInt >> 5)
	def lowDelay : Boolean = (toInt & 0x10) != 0
	def highThrougput : Boolean = (toInt & 0x8) != 0
	def highReliability : Boolean = (toInt & 0x4) != 0
}

object TypeOfService
{
	final case class Precedence(toInt : Int) extends AnyVal
	
	val Routine = Precedence(0)
	val Priority = Precedence(1)
	val Immediate = Precedence(2)
	val Flash = Precedence(3) 
	val FlashOverride = Precedence(4)
	val CriticECP = Precedence(5)
	val InternetworkControl = Precedence(6)
	val NetworkControl = Precedence(7)
}