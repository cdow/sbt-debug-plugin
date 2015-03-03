package com.github.cdow.responses

import com.github.cdow.PrimitiveCodecs._
import scodec.Codec
import scodec.bits.BitVector

case class IdSizes(fieldId: Int, methodId: Int, objectId: Int, referenceTypeId: Int, frameId: Int)
case class EventRequestSet(requestID: RequestId)

object ResponseCodecs {
	implicit val idSizes: Codec[IdSizes] = (int :: int :: int :: int :: int).as[IdSizes]
	implicit val eventRequestSet: Codec[EventRequestSet] = requestId.hlist.as[EventRequestSet]

	def decodeEventRequestSet(binPacket: Array[Byte]): EventRequestSet = {
		Codec.decodeValidValue[EventRequestSet](BitVector(binPacket))
	}

	def encodeEventRequestSet(setResponse: EventRequestSet): Array[Byte] = {
		Codec.encodeValid(setResponse).toByteArray
	}
}
