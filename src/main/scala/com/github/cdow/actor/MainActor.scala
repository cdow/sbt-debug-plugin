package com.github.cdow.actor

import akka.actor.{FSM, Props}
import akka.util.ByteString
import com.github.cdow.PrimitiveCodecs.RequestId
import com.github.cdow._
import com.github.cdow.actor.debugger.DebuggerActor
import com.github.cdow.actor.vm.{VmMessage, VmActor}
import com.github.cdow.commands._
import com.github.cdow.responses.{EventRequestSet, IdSizes}
import com.github.cdow.responses.ResponseCodecs._
import scodec.bits.ByteVector

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
				val encodedEvent = ByteString(JdwpCodecs.encodePacket(newVmEvent))
				self ! encodedEvent
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

			//TODO stop using null
			var responseCommand: CommandPacket = null

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
							val decodedResponse = decodeEventRequestSet(rp.data.toArray)
							val vmRequestId = decodedResponse.requestID
							eventRequestManager.eventRequestResponse(id, vmRequestId)
						case _ => // do nothing
					}

					responseCommand = cp

					awaitingReponse - id
			}

			val toSend = decoded

			val messageLens = shapeless.lens[JdwpPacket] >> 'message
			val commandLens = shapeless.lens[CommandPacket] >> 'command
			if(self == sender() || debuggerActor == sender()) {
				val clearLens = shapeless.lens[EventRequest.Clear] >> 'requestId
				val result = messageLens.modify(toSend) {
					case cp: CommandPacket =>
						commandLens.modify(cp) {
							case erc: EventRequest.Clear => clearLens.modify(erc) { requestId =>
								// If we don't have an active event for the requestId, mark it as a system
								// event (requestId 0) so the clear is ignored.
								// TODO make sure this is a good way to handle this
								eventRequestManager.toVmId(requestId).getOrElse(RequestId(0))
							}
							case other => other
						}
					case rp: ResponsePacket => rp
				}

println("DEBUGGER: " + result)
				vmActor ! ByteString(JdwpCodecs.encodePacket(result))
			} else {
				if(!isVmDeathEvent(toSend)) {
					val compositeLens = shapeless.lens[commands.Event.Composite] >> 'events
					val responseDataLens = shapeless.lens[ResponsePacket] >> 'data
					val eventRequestSetDataLens = shapeless.lens[EventRequestSet] >> 'requestID
					val result = messageLens.modify(toSend) {
						case cp: CommandPacket =>
							commandLens.modify(cp) {
								case ec: commands.Event.Composite =>
									compositeLens.modify(ec)(_.map {
										case event: VMStart => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: SingleStep => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: Breakpoint => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: MethodEntry => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: MethodExit => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: MethodExitWithReturnValue => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: MonitorContendedEnter => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: MonitorContendedEntered => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: MonitorWait => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: MonitorWaited => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: commands.Exception => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: ThreadStart => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: commands.ThreadDeath => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: ClassPrepare => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: ClassUnload => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: FieldAccess => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: FieldModification => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
										case event: VMDeath => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
									})
								case other => other
							}
						case rp: ResponsePacket =>
							responseCommand.command match {
								case _: EventRequest.Set =>
									responseDataLens.modify(rp) { data =>
										val parsedData = decodeEventRequestSet(data.toArray)
										val modifiedParsedData = eventRequestSetDataLens.modify(parsedData)(eventRequestManager.toDebuggerId)
										ByteVector(encodeEventRequestSet(modifiedParsedData))
									}
								case _ => rp
							}
					}

println("VM:       " + toSend)
					debuggerActor ! ByteString(JdwpCodecs.encodePacket(result))
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
