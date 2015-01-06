package com.github.cdow

import scodec.bits.{ByteVector, BitVector}
import scodec.codecs._
import com.github.cdow.responses._
import com.github.cdow.commands._
import scodec.Codec

object JdwpCodecs {
	val responsePacket: Codec[ResponsePacket] = {
		("errorCode" | uint16) ::
		("data" | bytes)
	}.as[ResponsePacket]
	
	implicit def jdwpPacket(implicit idSizes: IdSizes): Codec[JdwpPacket] = variableSizeBytes(int32, {
		("id" | uint32) ::
		discriminated[JdwpMessage].by(uint8)
			.\ (0) {case in: CommandPacket => in} (CommandCodecs.command.hlist.as[CommandPacket])
			.\ (0x80) {case in: ResponsePacket => in} (responsePacket)
	}, 4).as[JdwpPacket]

	def decodePacket(binPacket: Array[Byte])(implicit idSizes: IdSizes): JdwpPacket = {
		Codec.decodeValidValue[JdwpPacket](BitVector(binPacket))
	}
	
	def encodePacket(packet: JdwpPacket)(implicit idSizes: IdSizes): Array[Byte] = {
		Codec.encodeValid(packet).toByteArray
	}
}