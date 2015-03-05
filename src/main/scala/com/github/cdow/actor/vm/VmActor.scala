package com.github.cdow.actor.vm

import java.net.InetSocketAddress

import akka.actor.{FSM, ActorRef, Props}
import akka.io.{IO, Tcp}
import akka.io.Tcp._
import akka.util.ByteString
import com.github.cdow.actor.MainMessage
import com.github.cdow.actor.vm.VmConnection.Disconnected

sealed trait VmMessage
object VmMessage {
	case object Connect
	case object Disconnect
}

sealed trait VmState
object VmState {
	case object Idle extends VmState
	case object Binding extends VmState
	case object Bound extends VmState
	case object Connected extends VmState
	case object Running extends VmState
}

// TODO link this to VmState better
sealed trait VmConnection
object VmConnection {
	case object Disconnected extends VmConnection{
		def bind(binding: ActorRef): Bound = {
			Bound(binding)
		}
	}
	case class Bound(server: ActorRef) extends VmConnection {
		def unBind: Disconnected.type = {
			Disconnected
		}

		def connect(connection: ActorRef): Connected = {
			Connected(server, connection)
		}
	}
	case class Connected(server: ActorRef, connection: ActorRef) extends VmConnection {
		def unBind: Disconnected.type = {
			Disconnected
		}

		def disconnect: Bound = {
			Bound(server)
		}
	}
}

object VmActor {
	def props(port: Int, listener: ActorRef) = Props(new VmActor(port, listener))
}

class VmActor(port: Int, listener: ActorRef) extends FSM[VmState, VmConnection] {
	import context.system
	import VmState._
	
	val HANDSHAKE = ByteString.fromString("JDWP-Handshake", "US-ASCII")

	startWith(Idle, VmConnection.Disconnected)

	when(Idle) {
		case Event(VmMessage.Connect, VmConnection.Disconnected) =>
			IO(Tcp) ! Bind(self, new InetSocketAddress(port))
			goto(Binding)
	}

	when(Binding) {
		case Event(Tcp.Bound(localAddress), vmConn: VmConnection.Disconnected.type) =>
			val server = sender()
			goto(Bound) using vmConn.bind(server)
		case Event(CommandFailed(_: Bind), VmConnection.Disconnected) =>
			context stop self
			stay()
	}

	when(Bound) {
		case Event(Tcp.Connected(remote, local), vmConn: VmConnection.Bound) =>
			val connection = sender()
			connection ! Register(self)
			connection ! Write(HANDSHAKE)
			goto(Connected) using vmConn.connect(connection)
		case Event(VmMessage.Disconnect, vmConn: VmConnection.Bound) =>
			IO(Tcp) ! Unbind
			goto(Idle) using vmConn.unBind
	}

	when(Connected) {
		case Event(Received(HANDSHAKE), _: VmConnection.Connected) =>
			listener ! MainMessage.VmConnected
			goto(Running)
		case Event(_: ConnectionClosed, vmConn: VmConnection.Connected) =>
			listener ! MainMessage.VmDisconnected
			goto(Bound) using vmConn.disconnect
		case Event(VmMessage.Disconnect, vmConn: VmConnection.Connected) =>
			vmConn.connection ! Close
			vmConn.server ! Unbind
			goto(Idle) using vmConn.unBind
	}

	when(Running) {
		case Event(data: ByteString, vmConn: VmConnection.Connected) =>
			vmConn.connection ! Write(data)
			stay()
		case Event(Received(data), _: VmConnection.Connected) =>
			listener ! data
			stay()
		case Event(_: ConnectionClosed, vmConn: VmConnection.Connected) =>
			listener ! MainMessage.VmDisconnected
			goto(Bound) using vmConn.disconnect
		case Event(VmMessage.Disconnect, vmConn: VmConnection.Connected) =>
			vmConn.connection ! Close
			vmConn.server ! Unbind
			goto(Idle) using vmConn.unBind
	}

	initialize()
}

