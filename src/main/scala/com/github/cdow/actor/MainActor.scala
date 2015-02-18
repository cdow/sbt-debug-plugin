package com.github.cdow.actor

import akka.actor.{FSM, Props}
import akka.util.ByteString
import com.github.cdow.PrimitiveCodecs.RequestId
import com.github.cdow._
import com.github.cdow.actor.debugger.DebuggerActor
import com.github.cdow.actor.vm.{VmMessage, VmActor}
import com.github.cdow.commands.{EventRequest, VMDeath}
import com.github.cdow.responses.{ResponseCodecs, IdSizes}

trait MainState
object MainState {
	case object Idle extends MainState
	case object DebuggerConnected extends MainState
	case object VmConnected extends MainState
}

trait MainMessage
object MainMessage {
	case object DebuggerConnected extends MainMessage
	case object DebuggerDisconnected extends MainMessage
	case object VmConnected extends MainMessage
	case object VmDisconnected extends MainMessage
}

object MainActor {
	def props(debuggerPort: Int, vmPort: Int) = Props(new MainActor(debuggerPort, vmPort))
}

class MainActor(debuggerPort: Int, vmPort: Int) extends FSM[MainState, Unit] {
	import MainState._

	// TODO determine this dynamically
	implicit val idSizes = IdSizes(8,8,8,8,8)

	val debuggerActor = context.actorOf(DebuggerActor.props(debuggerPort, self), "debugger")
	val vmActor = context.actorOf(VmActor.props(vmPort, self), "vm")

	var queuedMessages = Seq.empty[ByteString]
	var awaitingReponse = Map.empty[Long, CommandPacket]
	val eventRequestManager = new EventRequestManager

	startWith(Idle, ())

	when(Idle) {
		case Event(MainMessage.DebuggerConnected, ()) =>
			vmActor ! VmMessage.Connect
			goto(DebuggerConnected)
	}

	when(DebuggerConnected) {
		case Event(data: ByteString, ()) =>
			queuedMessages = queuedMessages :+ data
			stay
		case Event(MainMessage.VmConnected, ()) =>
			// TODO keep response from these from going to the debugger
			val newVmEvents = eventRequestManager.newVm()
			newVmEvents.foreach { newVmEvent =>
				self ! newVmEvent
			}

			queuedMessages.foreach { queuedMessage =>
				self ! queuedMessage
			}
			queuedMessages = Seq.empty
			goto(VmConnected)
		case Event(MainMessage.DebuggerDisconnected, ()) =>
			eventRequestManager.clearAllEvents()
			vmActor ! VmMessage.Disconnect
			goto(Idle)
	}

	when(VmConnected) {
		case Event(data: ByteString, ()) =>
			val decoded = JdwpCodecs.decodePacket(data.toArray)
println("INITIAL:  " + decoded)

			awaitingReponse = decoded match {
				case JdwpPacket(id, cp :CommandPacket) =>
					cp.command match {
						case set :EventRequest.Set => eventRequestManager.newEventRequest(id, set)
						case EventRequest.Clear(eventKind, requestId) => eventRequestManager.clearEvent(requestId)
						case EventRequest.ClearAllBreakpoints => eventRequestManager.clearAllEvents()
						case _ => // do nothing
					}

					awaitingReponse + (id -> cp)
				case JdwpPacket(id, rp: ResponsePacket) =>
					val cp = awaitingReponse(id)
					cp.command match {
						case EventRequest.Set(_, _, _) =>
							val decodedResponse = ResponseCodecs.decodeEventRequestSet(rp.data.toArray)
							val vmRequestId = decodedResponse.requestID
							eventRequestManager.eventRequestResponse(id, vmRequestId)
						case _ => // do nothing
					}

					awaitingReponse - id
			}

			val toSend = decoded

			if(self == sender() || debuggerActor == sender()) {
println("DEBUGGER: " + toSend)
				vmActor ! ByteString(JdwpCodecs.encodePacket(toSend))
			} else {
println("VM:       " + toSend)
				if(!isVmDeathEvent(toSend)) {
					debuggerActor ! ByteString(JdwpCodecs.encodePacket(toSend))
				}
			}
			stay
		case Event(MainMessage.VmDisconnected, ()) =>
			// TODO handle case where there are still messages awaitingResponse, effects EventRequestManager
			goto(DebuggerConnected)
		case Event(MainMessage.DebuggerDisconnected, ()) =>
			eventRequestManager.clearAllEvents()
			vmActor ! VmMessage.Disconnect
			goto(Idle)
	}

	private def isVmDeathEvent(message: JdwpPacket): Boolean = {
		message.message match {
			case CommandPacket(commands.Event.Composite(_, events)) =>
				events.contains(VMDeath(RequestId(0)))
			case _ => false
		}
	}
}
