package com.github.cdow.actor.debugger

import java.net.InetSocketAddress

import akka.actor.{FSM, Props, ActorRef}
import akka.io.Tcp._
import akka.io.{Tcp, IO}
import akka.util.ByteString

sealed trait DebuggerState
object DebuggerState {
	case object Idle extends DebuggerState
	case object Bound extends DebuggerState
	case object Connected extends DebuggerState
	case object Running extends DebuggerState
}

object DebuggerActor {
	def props(port: Int, listener: ActorRef) = Props(new DebuggerActor(port, listener))
}

class DebuggerActor(port: Int, listener: ActorRef) extends FSM[DebuggerState, Option[ActorRef]] {
	import context.system
	import DebuggerState._

	val HANDSHAKE = ByteString.fromString("JDWP-Handshake", "US-ASCII")

	IO(Tcp) ! Bind(self, new InetSocketAddress(port))

	startWith(Idle, None)

	when(Idle) {
		case Event(Tcp.Bound(localAddress), None) =>
			goto(Bound)
		case Event(CommandFailed(_: Bind), None) =>
			context stop self
			stay
	}

	when(Bound) {
		case Event(Tcp.Connected(remote, local), None) =>
			listener ! MainMessage.DebuggerConnected
			val connection = sender()
			connection ! Register(self)
			goto(Connected) using Some(connection)
	}

	when(Connected) {
		case Event(Received(HANDSHAKE), Some(connection)) =>
			connection ! Write(HANDSHAKE)
			goto(Running)
		case Event(_ :ConnectionClosed, Some(_)) =>
			listener ! MainMessage.DebuggerDisconnected
			goto(Bound) using None
	}

	when(Running) {
		case Event(data: ByteString, Some(connection)) =>
			connection ! Write(data)
			stay
		case Event(Received(data), Some(_)) =>
			listener ! data
			stay
		case Event(_: ConnectionClosed, Some(_)) =>
			listener ! MainMessage.DebuggerDisconnected
			goto(Bound) using None
	}

	initialize()
}

