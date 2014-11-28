package com.github.cdow.actor.debugger

import java.net.InetSocketAddress

import akka.actor.{FSM, Props, ActorRef}
import akka.io.Tcp._
import akka.io.{Tcp, IO}
import akka.util.ByteString

sealed trait DebuggerState
case object Initial extends DebuggerState
case object Bound extends DebuggerState
case object Connected extends DebuggerState
case object Running extends DebuggerState

object DebuggerActor {
	def props(port: Int, listener: ActorRef) = Props(new DebuggerActor(port, listener))
}

class DebuggerActor(port: Int, listener: ActorRef) extends FSM[DebuggerState, Option[ActorRef]] {
	import context.system

	val HANDSHAKE = ByteString.fromString("JDWP-Handshake", "US-ASCII")

	IO(Tcp) ! Bind(self, new InetSocketAddress(port))

	startWith(Initial, None)

	when(Initial) {
		case Event(b @ Bound(localAddress)) =>
			goto(Bound)
		case Event(CommandFailed(_: Bind)) =>
			context stop self
			stay
	}

	when(Bound) {
		case Event(c @ Connected(remote, local)) =>
			listener ! c
			val connection = sender()
			connection ! Register(self)
			goto(Connected) using Some(connection)
	}

	when(Connected) {
		case Event(Received(data), connection: Option[ActorRef]) if data == HANDSHAKE =>
			connection.foreach(_ ! Write(HANDSHAKE))
			goto(Running)
		case Event(connectionClosed: ConnectionClosed) =>
			listener ! connectionClosed
			goto(Bound) using None
	}

	when(Running) {
		case Event(data: ByteString, Some(connection)) =>
			connection ! Write(data)
			stay
		case Event(Received(data)) =>
			listener ! data
			stay
		case Event(connectionClosed: ConnectionClosed) =>
			listener ! connectionClosed
			goto(Bound) using None
	}

	initialize()
}

