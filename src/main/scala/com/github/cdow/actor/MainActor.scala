package com.github.cdow.actor

import akka.actor.{FSM, Actor, Props}
import akka.util.ByteString
import com.github.cdow.PrimitiveCodecs.RequestId
import com.github.cdow._
import com.github.cdow.actor.debugger.DebuggerActor
import com.github.cdow.actor.vm.{VmMessage, VmActor}
import com.github.cdow.commands.VMDeath
import com.github.cdow.responses.IdSizes

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
			queuedMessages.foreach { queuedMessage =>
				val decoded = JdwpCodecs.decodePacket(queuedMessage.toArray)
				awaitingReponse = updateAwaitingResponse(awaitingReponse, decoded)

				vmActor ! queuedMessage
			}
			queuedMessages = Seq.empty
			goto(VmConnected)
		case Event(MainMessage.DebuggerDisconnected, ()) =>
			vmActor ! VmMessage.Disconnect
			goto(Idle)
	}

	when(VmConnected) {
		case Event(data: ByteString, ()) =>
			val decoded = JdwpCodecs.decodePacket(data.toArray)
			awaitingReponse = updateAwaitingResponse(awaitingReponse, decoded)

			if(debuggerActor == sender()) {
println("DEBUGGER: " + decoded)
				vmActor ! data
			} else {
println("VM:       " + decoded)
				if(!isVmDeathEvent(decoded)) {
					debuggerActor ! data
				}
			}
			stay
		case Event(MainMessage.VmDisconnected, ()) =>
			goto(DebuggerConnected)
		case Event(MainMessage.DebuggerDisconnected, ()) =>
			vmActor ! VmMessage.Disconnect
			goto(Idle)
	}

	private def updateAwaitingResponse(oldAwaitingResponse: Map[Long, CommandPacket], newMessage: JdwpPacket): Map[Long, CommandPacket] = {
		newMessage match {
			case JdwpPacket(id, cp :CommandPacket)=>
				oldAwaitingResponse + (id -> cp)
			case JdwpPacket(id, rp: ResponsePacket)=>
				val cp = awaitingReponse(id)
				oldAwaitingResponse - id
		}
	}

	private def isVmDeathEvent(message: JdwpPacket): Boolean = {
		message.message match {
			case CommandPacket(commands.Event.Composite(_, events)) =>
				events.contains(VMDeath(RequestId(0)))
			case _ => false
		}
	}
}
