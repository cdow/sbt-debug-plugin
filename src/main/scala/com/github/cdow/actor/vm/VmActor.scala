package com.github.cdow.actor.vm

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
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

class VmActor(port: Int, listener: ActorRef) extends Actor {

	connect

	def receive: Actor.Receive = {
		case CommandFailed(_: Connect) =>
			connect

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
				case _: ConnectionClosed =>
					context unbecome()
					connect
			}
	}

	private def connect: Unit =
		IO(Tcp) ! Connect(new InetSocketAddress(port))
}

