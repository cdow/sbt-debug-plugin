package com.github.cdow.actor.vm

import java.net.InetSocketAddress

import akka.actor.{FSM, Actor, ActorRef, Props}
import akka.io.{IO, Tcp}
import akka.io.Tcp._
import akka.util.ByteString

sealed trait VmState
case object Initial extends VmState
case object DebuggerConnected extends VmState
case object Connected extends VmState
case object Running extends VmState

object VmActor {
	def props(port: Int, listener: ActorRef) = Props(new VmActor(port, listener))
}

class VmActor(port: Int, listener: ActorRef) extends FSM[VmState, Option[ActorRef]] {
	val HANDSHAKE = ByteString.fromString("JDWP-Handshake", "US-ASCII")

	startWith(Initial, None)

	when(Initial) {
		case Event("debugger-connected") =>
			connect
			goto(DebuggerConnected)
	}

	when(DebuggerConnected) {
		case Event(CommandFailed(_: Connect)) =>
			connect
			stay()
		case Event(Connected(remote, local))	 =>
			listener ! "vm-connected"
			val connection = sender()
			connection ! Register(self)
			connection ! Write(HANDSHAKE)
			goto(Connected) using Some(connection)
		case Event("debugger-disconnected", Some(connection)) =>
			connection ! Close
			goto(Initial) using None
	}

	when(Connected) {
		case Event(Received(HANDSHAKE), Some(connection)) =>
			goto(Running)
		case Event(connectionClosed: ConnectionClosed) =>
			listener ! "vm-disconnected"
			goto(DebuggerConnected) using None
		case Event("debugger-disconnected", Some(connection)) =>
			connection ! Close
			goto(Initial) using None
	}

	when(Running) {
		case Event(data: ByteString, Some(connection)) =>
			connection ! Write(data)
			stay
		case Event(Received(data)) =>
			listener ! data
			stay
		case Event(connectionClosed: ConnectionClosed) =>
			listener ! "vm-disconnected"
			goto(DebuggerConnected) using None
		case Event("debugger-disconnected", Some(connection)) =>
			connection ! Close
			goto(Initial) using None
	}

	initialize()

	private def connect: Unit =
		IO(Tcp) ! Connect(new InetSocketAddress(port))
}

