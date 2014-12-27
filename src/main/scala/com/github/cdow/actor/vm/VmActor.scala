package com.github.cdow.actor.vm

import java.net.InetSocketAddress

import akka.actor.{FSM, ActorRef, Props}
import akka.io.{IO, Tcp}
import akka.io.Tcp._
import akka.util.ByteString
import com.github.cdow.actor.MainMessage

sealed trait VmMessage
object VmMessage {
	case object Connect
	case object Disconnect
}

sealed trait VmState
object VmState {
	case object Idle extends VmState
	case object Connecting extends VmState
	case object Connected extends VmState
	case object Running extends VmState
}

object VmActor {
	def props(port: Int, listener: ActorRef) = Props(new VmActor(port, listener))
}

class VmActor(port: Int, listener: ActorRef) extends FSM[VmState, Option[ActorRef]] {
	import context.system
	import VmState._
	
	val HANDSHAKE = ByteString.fromString("JDWP-Handshake", "US-ASCII")

	startWith(Idle, None)

	when(Idle) {
		case Event(MainMessage.DebuggerConnected, None) =>
			connect
			goto(Connecting)
	}

	when(Connecting) {
		case Event(CommandFailed(_: Connect), None) =>
			connect
			stay()
		case Event(Tcp.Connected(remote, local), None)	 =>
			val connection = sender()
			connection ! Register(self)
			connection ! Write(HANDSHAKE)
			goto(Connected) using Some(connection)
		case Event(MainMessage.DebuggerDisconnected, Some(connection)) =>
			connection ! Close
			goto(Idle) using None
	}

	when(Connected) {
		case Event(Received(HANDSHAKE), Some(connection)) =>
			listener ! MainMessage.VmConnected
			goto(Running)
		case Event(_: ConnectionClosed, Some(_)) =>
			listener ! MainMessage.VmDisconnected
			goto(Connecting) using None
		case Event(MainMessage.DebuggerDisconnected, Some(connection)) =>
			connection ! Close
			goto(Idle) using None
	}

	when(Running) {
		case Event(data: ByteString, Some(connection)) =>
			connection ! Write(data)
			stay
		case Event(Received(data), Some(_)) =>
			listener ! data
			stay
		case Event(_: ConnectionClosed, Some(_)) =>
			listener ! MainMessage.VmDisconnected
			goto(Connecting) using None
		case Event(MainMessage.DebuggerDisconnected, Some(connection)) =>
			connection ! Close
			goto(Idle) using None
	}

	initialize()

	private def connect: Unit =
		IO(Tcp) ! Connect(new InetSocketAddress(port))
}

