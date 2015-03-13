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
	// TODO distinguish between vm and debugger ids
	var awaitingReponse = Map.empty[Long, Command]
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

			val packetInfo = toPacketInfo(decoded)
println("INITIAL:  " + packetInfo)
			awaitingReponse = packetInfo match {
				case CommandInfo(id, command) =>
					awaitingReponse + (id -> command)
				case ResponseInfo(id, _, _, _) =>
					awaitingReponse - id
			}

			packetInfo match {
				case CommandInfo(id, command) =>
					command match {
						case set :EventRequest.Set => eventRequestManager.newEventRequest(id, set)
						case EventRequest.Clear(eventKind, requestId) => eventRequestManager.clearEvent(requestId)
						case EventRequest.ClearAllBreakpoints => eventRequestManager.clearAllEvents()
						case _ => // do nothing
					}
				case ResponseInfo(id, _, data, command) =>
					command match {
						case EventRequest.Set(_, _, _) =>
							val decodedResponse = decodeEventRequestSet(data.toArray)
							val vmRequestId = decodedResponse.requestID
							eventRequestManager.eventRequestResponse(id, vmRequestId)
						case _ => // do nothing
					}
			}

			if(self == sender() || debuggerActor == sender()) {
				val result = convertToVmRequestIds(packetInfo)

println("DEBUGGER: " + result)
				vmActor ! ByteString(JdwpCodecs.encodePacket(result.toJdwpPacket))
			} else {
				if(!isVmDeathEvent(decoded)) {
					val result = convertToDebuggerRequestIds(packetInfo)

println("VM:       " + result)
					debuggerActor ! ByteString(JdwpCodecs.encodePacket(result.toJdwpPacket))
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

	// TODO report all classes as unloaded on VmDeath
	private def isVmDeathEvent(message: JdwpPacket): Boolean = {
		message.message match {
			case CommandPacket(commands.Event.Composite(_, events)) =>
				events.contains(VMDeath(RequestId(0)))
			case _ => false
		}
	}

	trait PacketInfo {
		def toJdwpPacket: JdwpPacket
	}
	case class ResponseInfo(id: Long, errorCode: Int, data: ByteVector, originalCommand: Command) extends PacketInfo {
		override def toJdwpPacket: JdwpPacket = JdwpPacket(id, ResponsePacket(errorCode, data))
	}
	case class CommandInfo(id: Long, command: Command) extends PacketInfo {
		override def toJdwpPacket: JdwpPacket = JdwpPacket(id, CommandPacket(command))
	}

	def toPacketInfo(packet: JdwpPacket): PacketInfo = packet match {
		case JdwpPacket(id, cp :CommandPacket) => CommandInfo(id, cp.command)
		case JdwpPacket(id, rp: ResponsePacket) =>
			val command = awaitingReponse(id)
			ResponseInfo(id, rp.errorCode, rp.data, command)
	}

	// These requestId conversions would be so much easier if the compiler didn't choke on shapeless.everything
	private val messageLens = shapeless.lens[JdwpPacket] >> 'message
	private val commandLens = shapeless.lens[CommandInfo] >> 'command
	private def convertToDebuggerRequestIds(packetInfo: PacketInfo): PacketInfo = {
		val compositeLens = shapeless.lens[commands.Event.Composite] >> 'events
		val responseInfoDataLens = shapeless.lens[ResponseInfo] >> 'data
		val eventRequestSetDataLens = shapeless.lens[EventRequestSet] >> 'requestID
		packetInfo match {
			case ci: CommandInfo =>
				commandLens.modify(ci) {
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
			case ri: ResponseInfo =>
				ri.originalCommand match {
					case _: EventRequest.Set =>
						responseInfoDataLens.modify(ri) { data =>
							val parsedData = decodeEventRequestSet(data.toArray)
							val modifiedParsedData = eventRequestSetDataLens.modify(parsedData)(eventRequestManager.toDebuggerId)
							ByteVector(encodeEventRequestSet(modifiedParsedData))
						}
					case _ => ri
				}
		}
	}

	private def convertToVmRequestIds(packetInfo: PacketInfo): PacketInfo = {
		val clearLens = shapeless.lens[EventRequest.Clear] >> 'requestId
		packetInfo match {
			case ci: CommandInfo =>
				commandLens.modify(ci) {
					case erc: EventRequest.Clear => clearLens.modify(erc) { requestId =>
						// If we don't have an active event for the requestId, mark it as a system
						// event (requestId 0) so the clear is ignored.
						// TODO make sure this is a good way to handle this
						eventRequestManager.toVmId(requestId).getOrElse(RequestId(0))
					}
					case other => other
				}
			case ri: ResponseInfo => ri
		}
	}
}
