package com.github.cdow

import com.github.cdow.PrimitiveCodecs.ReferenceTypeId
import com.github.cdow.responses.IdSizes
import scodec.bits.ByteVector

import scala.annotation.tailrec

class ReferenceTypeIdManager(idSizes: IdSizes) {
	// TODO return errors instead of putting in an incorrect id
	val errorId = ReferenceTypeId(ByteVector.high(referenceTypeIdSize))
	val referenceTypeIdSize = idSizes.referenceTypeId
	val nullId = ReferenceTypeId(ByteVector.low(referenceTypeIdSize))
	var referenceTypeIdCount = increment(ByteVector.low(referenceTypeIdSize)) // increment because low is reserved for 'null'
	var referenceTypeIdMapping = Map.empty[ReferenceTypeId, ReferenceTypeId] // VM -> debugger

	def toDebuggerId(vmId: ReferenceTypeId): ReferenceTypeId = {
		if(vmId == nullId) {
			vmId
		} else {
			referenceTypeIdMapping.getOrElse(vmId, {
				val newDebuggerId = ReferenceTypeId(referenceTypeIdCount)

				referenceTypeIdCount = increment(referenceTypeIdCount)
				referenceTypeIdMapping = referenceTypeIdMapping + (vmId -> newDebuggerId)

				newDebuggerId
			})
		}
	}

	def toVmId(debuggerId: ReferenceTypeId): ReferenceTypeId = {
		if(debuggerId == nullId) {
			debuggerId
		} else {
			referenceTypeIdMapping.find(_._2 == debuggerId).map(_._1).getOrElse(errorId)
		}
	}

	def newVm(): Unit = {
		referenceTypeIdMapping = Map.empty
	}

	private def increment(start: ByteVector): ByteVector = {
		val (result, overflow) = start.foldRight((ByteVector.empty, true)) { (byte, progress) =>
			val (incrementedAlready , carry) = progress
			val incremented = if(carry) {
				(byte + 1).asInstanceOf[Byte]
			} else {
				byte
			}

			(incremented +: incrementedAlready, carry && incremented == 0)
		}

		result
	}

	@tailrec
	private def greaterThan(first: ByteVector, second: ByteVector): Boolean = {
		if(first.length != second.length) {
			throw new IllegalArgumentException("Can't compare values for inputs that are not the same length")
		}

		if(first.isEmpty) {
			false
		} else if(first.head > second.head) {
			true
		} else if(first.head < second.head) {
			false
		} else {
			greaterThan(first.tail, second.tail)
		}
	}
}
