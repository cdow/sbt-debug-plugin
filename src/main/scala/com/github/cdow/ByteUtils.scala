package com.github.cdow

import java.nio.ByteBuffer

object ByteUtils {
	def bytesToInt(bytes: Array[Byte]): Int = {
		ByteBuffer.wrap(bytes).getInt
	}

	def intToBytes(integer: Int): Array[Byte] = {
		ByteBuffer.allocate(4).putInt(integer).array
	}

	def bytesToShort(bytes: Array[Byte]): Short = {
		ByteBuffer.wrap(bytes).getShort
	}

	def shortToBytes(short: Short): Array[Byte] = {
		ByteBuffer.allocate(2).putShort(short).array
	}
}