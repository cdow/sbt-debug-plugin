package com.github.cdow.responses

import com.github.cdow.PrimitiveCodecs._
import scodec.Codec

case class IdSizes(fieldId: Int, methodId: Int, objectId: Int, referenceTypeId: Int, frameId: Int)
case class EventRequestSet(requestID: Int)

object ResponseCodecs {
	val idSizes: Codec[IdSizes] = (int :: int :: int :: int :: int).as[IdSizes]
	val eventRequestSet: Codec[EventRequestSet] = int.hlist.as[EventRequestSet]
}
