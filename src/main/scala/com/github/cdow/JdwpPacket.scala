package com.github.cdow
import ByteUtils._

object JdwpPacket {
	val responseFlag: Byte = 0x80.toByte

	def apply(packetBytes: Array[Byte]): JdwpPacket = {
		val length = bytesToInt(packetBytes.slice(0, 4))
		val id = bytesToInt(packetBytes.slice(4, 8))
		val flags = packetBytes(8)

		if (flags == responseFlag) {
			val errorCode = bytesToShort(packetBytes.slice(9, 11))
			val data = packetBytes.drop(11)

			ResponsePacket(length, id, flags, errorCode, data)
		} else {
			val commandSet = packetBytes(9)
			val command = packetBytes(10)
			val data = packetBytes.drop(11)

			CommandPacket(length, id, flags, commandSet, command, data)
		}
	}
}

sealed trait JdwpPacket {
	def length: Int
	def id: Int
	def flags: Byte
	def data: Array[Byte]

	def toBytes: Array[Byte]
}

case class CommandPacket(
	length: Int,
	id: Int,
	flags: Byte,
	commandSet: Byte,
	command: Byte,
	data: Array[Byte]) extends JdwpPacket {
	def toBytes: Array[Byte] = {
		(ByteUtils.intToBytes(length) ++ ByteUtils.intToBytes(id) :+ flags :+ commandSet :+ command) ++ data
	}
}

case class ResponsePacket(
	length: Int,
	id: Int,
	flags: Byte,
	errorCode: Short,
	data: Array[Byte]) extends JdwpPacket {
	def toBytes: Array[Byte] = {
		(ByteUtils.intToBytes(length) ++ ByteUtils.intToBytes(id) :+ flags) ++ ByteUtils.shortToBytes(errorCode) ++ data
	}
}