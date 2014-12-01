package com.github.cdow.actor

import akka.actor.{FSM, Actor, Props}
import akka.util.ByteString
import com.github.cdow.JdwpCodecs
import com.github.cdow.actor.debugger.DebuggerActor
import com.github.cdow.actor.vm.VmActor

trait MainState
object MainState {
	case object Initial extends MainState
	case object DebuggerConnected extends MainState
	case object VmConnected extends MainState
}

trait MainMessages
object MainMessages {
	case object DebuggerConnected extends MainMessages
	case object DebuggerDisconnected extends MainMessages
	case object VmConnected extends MainMessages
	case object VmDisconnected extends MainMessages
}

object MainActor {
	def props(debuggerPort: Int, vmPort: Int) = Props(new MainActor(debuggerPort, vmPort))
}

class MainActor(debuggerPort: Int, vmPort: Int) extends FSM[MainState, Unit] {
	val debuggerActor = context.actorOf(DebuggerActor.props(debuggerPort, self), "debugger")
	val vmActor = context.actorOf(VmActor.props(vmPort, self), "vm")

	var queuedMessages = Seq.empty[ByteString]

	startWith(MainState.Initial, ())

	when(MainState.Initial) {
		case Event(MainMessages.DebuggerConnected, ()) =>
			vmActor ! MainMessages.DebuggerConnected
			goto(MainState.DebuggerConnected)
	}

	when(MainState.DebuggerConnected) {
		case Event(data: ByteString, ()) =>
			queuedMessages = queuedMessages :+ data
			stay
		case Event(MainMessages.VmConnected, ()) =>
			queuedMessages.foreach(vmActor ! _)
			queuedMessages = Seq.empty
			goto(MainState.VmConnected)
		case Event(MainMessages.DebuggerDisconnected, ()) =>
			vmActor ! MainMessages.DebuggerDisconnected
			goto(MainState.Initial)
	}

	when(MainState.VmConnected) {
		case Event(data: ByteString, ()) =>
			if(debuggerActor == sender()) {
				val decoded = JdwpCodecs.decodePacket(data.toArray)
println("DEBUGGER: " + decoded)
				vmActor ! data
			} else {
				val decoded = JdwpCodecs.decodePacket(data.toArray)
println("VM:       " + decoded)
				debuggerActor ! data
			}
			stay
		case Event(MainMessages.VmDisconnected, ()) =>
			goto(MainState.DebuggerConnected)
		case Event(MainMessages.DebuggerDisconnected, ()) =>
			vmActor ! MainMessages.DebuggerDisconnected
			goto(MainState.Initial)
	}
}
