package com.github.cdow

import java.nio.ByteBuffer

object ByteUtils {
	def bytesToInt(bytes: Array[Byte]): Int = {
		ByteBuffer.wrap(bytes).getInt
	}
}