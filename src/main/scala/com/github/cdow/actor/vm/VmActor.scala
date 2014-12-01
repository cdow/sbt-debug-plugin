package com.github.cdow.actor.vm

import java.net.InetSocketAddress

import akka.actor.{FSM, ActorRef, Props}
import akka.io.{IO, Tcp}
import akka.io.Tcp._
import akka.util.ByteString
import com.github.cdow.actor.MainMessages

sealed trait VmState
case object Initial extends VmState
case object DebuggerConnected extends VmState
case object Connected extends VmState
case object Running extends VmState

object VmActor {
	def props(port: Int, listener: ActorRef) = Props(new VmActor(port, listener))
}

class VmActor(port: Int, listener: ActorRef) extends FSM[VmState, Option[ActorRef]] {
	import context.system
	
	val HANDSHAKE = ByteString.fromString("JDWP-Handshake", "US-ASCII")

	startWith(Initial, None)

	when(Initial) {
		case Event(MainMessages.DebuggerConnected, None) =>
			connect
			goto(DebuggerConnected)
	}

	when(DebuggerConnected) {
		case Event(CommandFailed(_: Connect), None) =>
			connect
			stay()
		case Event(Tcp.Connected(remote, local), None)	 =>
			val connection = sender()
			connection ! Register(self)
			connection ! Write(HANDSHAKE)
			goto(Connected) using Some(connection)
		case Event(MainMessages.DebuggerDisconnected, Some(connection)) =>
			connection ! Close
			goto(Initial) using None
	}

	when(Connected) {
		case Event(Received(HANDSHAKE), Some(connection)) =>
			listener ! MainMessages.VmConnected
			goto(Running)
		case Event(_: ConnectionClosed, Some(_)) =>
			listener ! MainMessages.VmDisconnected
			goto(DebuggerConnected) using None
		case Event(MainMessages.DebuggerDisconnected, Some(connection)) =>
			connection ! Close
			goto(Initial) using None
	}

	when(Running) {
		case Event(data: ByteString, Some(connection)) =>
			connection ! Write(data)
			stay
		case Event(Received(data), Some(_)) =>
			listener ! data
			stay
		case Event(_: ConnectionClosed, Some(_)) =>
			listener ! MainMessages.VmDisconnected
			goto(DebuggerConnected) using None
		case Event(MainMessages.DebuggerDisconnected, Some(connection)) =>
			connection ! Close
			goto(Initial) using None
	}

	initialize()

	private def connect: Unit =
		IO(Tcp) ! Connect(new InetSocketAddress(port))
}

