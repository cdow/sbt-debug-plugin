package com.github.cdow

import scodec.bits.{ByteVector, BitVector}
import scodec.codecs._
import com.github.cdow.responses._
import scodec.Codec

object JdwpCodecs {
	implicit val idSizeResponse: Codec[IdSizes] = fixedSizeBytes(20, {
		("fieldIdSize" | int32) ::
		("methodIdSize" | int32) ::
		("objectIdSize" | int32) ::
		("referenceTyepIdSize" | int32) ::
		("frameIdSize" | int32)
	}).as[IdSizes]
	
	def parseIdSizeResponse(binPacket: Array[Byte]): IdSizes = {
		Codec.decodeValidValue[IdSizes](BitVector(binPacket))
	}
}