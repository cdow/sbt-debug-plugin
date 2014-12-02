package com.github.cdow.actor

import akka.actor.{FSM, Actor, Props}
import akka.util.ByteString
import com.github.cdow.JdwpCodecs
import com.github.cdow.actor.debugger.DebuggerActor
import com.github.cdow.actor.vm.{VmMessage, VmActor}

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
	
	val debuggerActor = context.actorOf(DebuggerActor.props(debuggerPort, self), "debugger")
	val vmActor = context.actorOf(VmActor.props(vmPort, self), "vm")

	var queuedMessages = Seq.empty[ByteString]

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
			queuedMessages.foreach(vmActor ! _)
			queuedMessages = Seq.empty
			goto(VmConnected)
		case Event(MainMessage.DebuggerDisconnected, ()) =>
			vmActor ! VmMessage.Disconnect
			goto(Idle)
	}

	when(VmConnected) {
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
		case Event(MainMessage.VmDisconnected, ()) =>
			goto(DebuggerConnected)
		case Event(MainMessage.DebuggerDisconnected, ()) =>
			vmActor ! VmMessage.Disconnect
			goto(Idle)
	}
}
