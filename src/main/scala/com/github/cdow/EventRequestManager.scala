package com.github.cdow

import com.github.cdow.PrimitiveCodecs.RequestId
import com.github.cdow.commands._

class EventRequestManager {
	private var idMappings = Seq.empty[RequestIdMapping]
	private var pendingEventRequests = Map.empty[Long, PendingRequestIdMapping]
	private var requestIdSequence: Int = 1
	// TODO ensure packet ids are unique
	private var packetIdSequence: Long = Int.MaxValue / 2

	def newEventRequest(commandId: Long, eventRequest: EventRequest.Set): Unit = {
		// prevent overwriting of requestIds for for new VM event replay
		if(!pendingEventRequests.contains(commandId)) {
			pendingEventRequests = pendingEventRequests + (commandId -> PendingRequestIdMapping(commandId, newDebuggerRequestId(), eventRequest))
		}
	}

	def eventRequestResponse(commandId: Long, vmId: RequestId): Unit = {
		pendingEventRequests.get(commandId).foreach { pendingMapping =>
			pendingEventRequests = pendingEventRequests - commandId
			// remove old id mappings if the VM is reusing a requestId.  This means that the old event is no longer valid
			idMappings = idMappings.filterNot(_.vmId == vmId)
			idMappings = idMappings :+ RequestIdMapping(vmId, pendingMapping.debuggerId, pendingMapping.eventRequest)
		}
	}

	// TODO we assume the event kind always matches, can we do this
	def clearEvent(debuggerId: RequestId): Unit = {
		idMappings = idMappings.filterNot(_.debuggerId == debuggerId)
	}

	def clearAllEvents(): Unit = {
		idMappings = Seq.empty
		pendingEventRequests = Map.empty
	}

	def newVm(): Seq[JdwpPacket] = {
		// We can't reuse event requests that reference vm specific ids, so we just drop these
		val filteredIdMappings = idMappings.filter { case RequestIdMapping(vmId, debuggerId, eventRequest) =>
			eventRequest.modifiers.find { requestModifier =>
				requestModifier.isInstanceOf[Conditional] ||
				requestModifier.isInstanceOf[ThreadOnly] ||
				requestModifier.isInstanceOf[ClassOnly] ||
				requestModifier.isInstanceOf[LocationOnly] ||
				requestModifier.isInstanceOf[ExceptionOnly] ||
				requestModifier.isInstanceOf[FieldOnly] ||
				requestModifier.isInstanceOf[Step] ||
				requestModifier.isInstanceOf[InstanceOnly]
			}.isEmpty
		}

		val newPendingEventRequests = filteredIdMappings.map { case RequestIdMapping(vmId, debuggerId, eventRequest) =>
			val packetId = packetIdSequence
			packetIdSequence += 1

			packetId -> PendingRequestIdMapping(packetId, debuggerId, eventRequest)
		}.toMap

		val replayCommands = newPendingEventRequests.values.map { case  PendingRequestIdMapping(packetId, debuggerId, eventRequest) =>
			JdwpPacket(packetId, CommandPacket(eventRequest))
		}

		pendingEventRequests = newPendingEventRequests
		idMappings = Seq.empty

		replayCommands.toSeq
	}

	def toDebuggerId(vmId: RequestId): RequestId = {
		if(vmId.id == 0) {
			vmId
		} else {
			// get should always succeed since we should have all requests mapped
			idMappings.find(_.vmId == vmId).get.debuggerId
		}
	}

	def toVmId(debuggerId: RequestId): Option[RequestId] = {
		// this fails to find anything if it is an id for an event that doesn't apply to the current VM
		idMappings.find(_.debuggerId == debuggerId).map(_.vmId)
	}

	private def newDebuggerRequestId(): RequestId = {
		requestIdSequence += 1
		RequestId(requestIdSequence)
	}

	private case class RequestIdMapping(vmId: RequestId, debuggerId: RequestId, eventRequest: EventRequest.Set)
	private case class PendingRequestIdMapping(commandId: Long, debuggerId: RequestId, eventRequest: EventRequest.Set)
}
