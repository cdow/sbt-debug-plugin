package com.github.cdow.actor.debugger

import java.net.InetSocketAddress

import akka.actor.{Actor, Props, ActorRef}
import akka.io.Tcp._
import akka.io.{Tcp, IO}
import akka.util.ByteString

sealed trait DebuggerState
case object Initial extends DebuggerState
case object Connected extends DebuggerState
case object Running extends DebuggerState

object DebuggerActor {
	def props(port: Int, listener: ActorRef) = Props(new DebuggerActor(port, listener))
}

class DebuggerActor(port: Int, listener: ActorRef) extends Actor {
	import context.system

	IO(Tcp) ! Bind(self, new InetSocketAddress(port))

	def receive: Actor.Receive = {
		case b @ Bound(localAddress) =>
		// do some logging or setup ...

		case CommandFailed(_: Bind) => context stop self

		case c @ Connected(remote, local) =>
			listener ! c
			val connection = sender()
			connection ! Register(self)
			context become {
				case data: ByteString =>
					connection ! Write(data)
				case Received(data) =>
					listener ! data
				case "close" =>
					connection ! Close
				case connectionClosed: ConnectionClosed =>
					context unbecome()
					listener ! connectionClosed
			}
	}
}

