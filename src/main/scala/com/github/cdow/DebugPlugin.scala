package com.github.cdow

import sbt._
import sbt.Keys._
import java.net.Socket
import java.net.ServerSocket
import java.nio.ByteBuffer
import scala.util.Try
import java.io.InputStream
import java.io.BufferedInputStream
import java.util.Arrays
import ByteUtils._
import com.github.cdow.responses._
import com.github.cdow.commands._
import scodec.bits.ByteVector

object DebugPlugin extends Plugin {
	val debugStart = taskKey[Unit]("starts debugger proxy")
	val debugStop = taskKey[Unit]("stops debugger proxy")
	val debugPort = settingKey[Int]("port to attache your debugger to")
	val debugVmPorts = settingKey[Set[Int]]("VM debug ports to connect to")

	private val debugProxyAttribute = AttributeKey[DebugProxy]("debugProxy")

	val debugSettings = Seq(
		debugPort := 6006,
		debugVmPorts := Set(6007),
		onLoad in Global := {
			(onLoad in Global).value.andThen { state =>
				val proxy = new DebugProxy(debugPort.value, debugVmPorts.value)
				state.put(debugProxyAttribute, proxy)
			}
		},
		onUnload in Global := {
			(onUnload in Global).value.andThen { state =>
				state.get(debugProxyAttribute).foreach { proxy =>
					proxy.stop
				}
				state.remove(debugProxyAttribute)
			}
		},
		debugStart := {
			state.value.get(debugProxyAttribute).foreach { proxy =>
				proxy.start
			}
		},
		debugStop := {
			state.value.get(debugProxyAttribute).foreach { proxy =>
				proxy.stop
			}
		})

	case class BreakPoint(requestId: Integer, originalMessage: JdwpPacket)

	class DebugProxy(port: Int, val vmPorts: Set[Int]) {
		val HANDSHAKE = "JDWP-Handshake".toCharArray.map(_.toByte)
		val ID_SIZE_REQUEST = Array[Byte](0, 0, 0, 11, 127, 127, 127, 127, 0, 1, 7)
		val ID_SIZE_REQUEST_ID = Array[Byte](127, 127, 127, 127) // TODO think about unlikely request id
		private var server: ServerSocket = null
		private var thread: DebugThread = null
		private var connectedVms = Map[Int, Socket]()
		private var runThread = true

		private var replayMessages = Vector[BreakPoint]()
		private var nextRequestId = 0

		def stop(): Unit = {
			runThread = false
			server.close
			connectedVms.values.foreach(_.close)
			connectedVms = Map[Int, Socket]()
			replayMessages = Vector[BreakPoint]()
		}

		def start(): Unit = {
			runThread = true
			server = new ServerSocket(port)
			thread = new DebugThread()

			thread.start
		}

		private def getNewVmSockets(): Map[Int, Socket] = {
			val unConnectedVmPorts = vmPorts -- connectedVms.keys
			val connectionAttempts = unConnectedVmPorts.map { port =>
				val attempt = Try(new Socket(null: String, port))

				(port, attempt)
			}.toMap

			val successfulConnections = connectionAttempts.filter {
				case (port, attempt) =>
					attempt.isSuccess
			}

			val newSocketMap = successfulConnections.mapValues(_.get)

			newSocketMap.values.foreach { socket =>
				socket.getOutputStream.write(HANDSHAKE)
				socket.getInputStream.skip(HANDSHAKE.length)
			}

			newSocketMap
		}

		private def getIdSizes(vmSocket: Socket): IdSizes = {
			vmSocket.getOutputStream.write(ID_SIZE_REQUEST)
			var response: JdwpPacket = null
			while (response == null) {
				val responseMessage = readMessage(vmSocket.getInputStream)
				if (responseMessage != null && bytesToInt(ID_SIZE_REQUEST_ID) == responseMessage.id) {
					response = responseMessage
				}
			}

			// TODO error handling better
			response match {
				case JdwpPacket(id, ResponsePacket(errors, data)) => JdwpCodecs.parseIdSizeResponse(data)
				case _ => throw new Exception("this is not an id size reponse")
			}
		}

		private def readMessage(inputStream: InputStream): JdwpPacket = {
			if (inputStream.available > 4) {
				val lengthBytes = Array.ofDim[Byte](4)
				inputStream.read(lengthBytes)

				val messageLength = bytesToInt(lengthBytes)

				val bodyBytes = Array.ofDim[Byte](messageLength - 4)
				inputStream.read(bodyBytes)

				JdwpCodecs.decodePacket(lengthBytes ++ bodyBytes)
			} else {
				// TODO stop using null
				null
			}
		}

		private def handleBreakPointMessage(packet: JdwpPacket): Unit = {
			if (packet != null) {
				packet.message match {
					case cp: CommandPacket =>
						cp.command match {
							// set
							case EventRequest.Set(_) =>
								val requestId = nextRequestId
								nextRequestId += 1
								replayMessages = replayMessages :+ BreakPoint(requestId, packet)
							// clear
							case EventRequest.Clear(_) =>
								// TODO make clear work properly
								val requestId = nextRequestId
								nextRequestId += 1
								replayMessages = replayMessages :+ BreakPoint(requestId, packet)
							// clear all
							case EventRequest.ClearAllBreakpoints(_) => replayMessages = Vector[BreakPoint]()
							case _ => // do nothing
						}
					case rp: ResponsePacket => // do nothing
				}
			}
		}

		private def peak(inStream: InputStream): Byte = {
			inStream.mark(1)
			val firstByte = inStream.read.asInstanceOf[Byte]
			inStream.reset

			firstByte
		}

		private class DebugThread extends Thread {
			override def run(): Unit = {
				while (runThread) {
					val debuggerSocket = server.accept

					val debuggerInput = debuggerSocket.getInputStream
					val debuggerOutput = debuggerSocket.getOutputStream
					while (debuggerInput.available < HANDSHAKE.length) {
						Thread.sleep(100)
					}
					debuggerInput.skip(HANDSHAKE.length)
					debuggerOutput.write(HANDSHAKE)

					val debuggerReadThread = new DebuggerReadThread(debuggerSocket)
					debuggerReadThread.start

					while (!debuggerSocket.isClosed) {
						// invalidate closed VM connections
						connectedVms = connectedVms.filter {
							case (port, socket) =>
								!socket.isClosed
						}

						// look for new VM connections
						val newConnectedVms = getNewVmSockets
						newConnectedVms.values.foreach { vmSocket =>
							val idSizes = getIdSizes(vmSocket)

							replayMessages.foreach { breakPoint =>
println("BREAKPOINT: " + breakPoint)
								vmSocket.getOutputStream.write(JdwpCodecs.encodePacket(breakPoint.originalMessage))
							}

							val vmReadThread = new VmReadThread(debuggerSocket, vmSocket, idSizes)
							vmReadThread.start
						}
						connectedVms = connectedVms ++ newConnectedVms

						Thread.sleep(100)
					}

					debuggerSocket.close

					replayMessages = Vector[BreakPoint]()

					connectedVms.foreach {
						case (port, socket) =>
							socket.close
					}
					connectedVms = Map[Int, Socket]()
				}
			}
		}

		private class DebuggerReadThread(debuggerSocket: Socket) extends Thread {
			override def run: Unit = {
				val inputStream = new BufferedInputStream(debuggerSocket.getInputStream)

				while (!debuggerSocket.isClosed) {
					val firstByte = peak(inputStream)
					if (firstByte == -1) {
						debuggerSocket.close
					} else {
						// send messages from debugger to VMs
						if (connectedVms.size > 0) {
							val debuggerMessage = readMessage(inputStream)
if (debuggerMessage != null) println("DEBUGGER: " + debuggerMessage)
							connectedVms.values.foreach { vmSocket =>
								val vmOutputStream = vmSocket.getOutputStream
								vmOutputStream.write(JdwpCodecs.encodePacket(debuggerMessage))
							}

							// save breakpoint messages
							handleBreakPointMessage(debuggerMessage)
						}
					}
				}
			}
		}

		private class VmReadThread(debuggerSocket: Socket, vmSocket: Socket, idSizes: IdSizes) extends Thread {
			private val ID_SIZE_REQUEST = Array[Byte](0, 0, 0, 11, 0, 0, 0, 1, 0, 1, 7)

			// map from original requestId to our fake requestId
			private var requestIdMap = Map.empty[Int, Int]

			override def run: Unit = {
				val inputStream = new BufferedInputStream(vmSocket.getInputStream)
				while (!vmSocket.isClosed) {
					val firstByte = peak(inputStream)
					if (firstByte == -1) {
						vmSocket.close
					} else {
						// send messages from VMs to debugger
						val debuggerOutputStream = debuggerSocket.getOutputStream
						val vmMessage = readMessage(inputStream)
if (vmMessage != null) println("ORIGINAL VM: " + vmMessage)
						val vmMessageReplacedId = replaceRequestId(vmMessage)
//						val vmMessageReplacedId = vmMessage
						if(!isVmDeathEvent(vmMessageReplacedId)) {
if (vmMessage != null) println("VM: " + vmMessageReplacedId)
							debuggerOutputStream.write(JdwpCodecs.encodePacket(vmMessageReplacedId))
						}
					}
				}
			}

			//TODO parse this properly
			private def isVmDeathEvent(message: JdwpPacket): Boolean = {
				message.message match {
					case CommandPacket(Event.Composite(data)) =>
						data.length == 10 && data(5) == (99: Byte)
					case _ => false
				}
			}

			private def replaceRequestId(packet: JdwpPacket): JdwpPacket = {
				packet.copy(message = packet.message match {
					case rp: ResponsePacket =>
						val breakPoint = replayMessages.find(_.originalMessage.id == packet.id)

						// TODO don't cast and handle errors
						breakPoint.filter(_.originalMessage.message.asInstanceOf[CommandPacket].command.isInstanceOf[EventRequest.Set] && rp.errorCode == 0).map { bp =>
							val requestId = bp.requestId
							val originalRequestId = bytesToInt(rp.data.toArray)
							requestIdMap = requestIdMap + (originalRequestId -> requestId)

							rp.copy(data = ByteVector.fromInt(requestId))
						}.getOrElse(rp)
					case CommandPacket(Event.Composite(data)) =>
						// breakpoint triggered
						val prefix = data.take(5) // includes some data
						val restData = data.slice(5, data.length)

						CommandPacket(Event.Composite(prefix ++ replaceCompositeRequestIds(restData)))
					case _ => packet.message
				})
			}

			val longSize = 8
			val intSize = 4
			val booleanSize = 1
			val byteSize = 1
			val charSize = 2
			val shortSize = 2
			val voidSize = 0
			val doubleSize = 8
			val floatSize = 4

			private def replaceCompositeRequestIds(data: ByteVector): ByteVector = {
				if (data.isEmpty) {
					ByteVector.empty
				} else {
					val requestIdSize = 4
					val threadIdSize = idSizes.objectId
					val locationSize = 1 + idSizes.objectId + idSizes.methodId + 8
					val taggedObjectId = 1 + idSizes.objectId

					val dataType = data.head
					val restData = data.tail
					val length = (dataType match {
						case VM_START => requestIdSize + threadIdSize
						case SINGLE_STEP => requestIdSize + threadIdSize + locationSize
						case BREAKPOINT => requestIdSize + threadIdSize + locationSize
						case METHOD_ENTRY => requestIdSize + threadIdSize + locationSize
						case METHOD_EXIT => requestIdSize + threadIdSize + locationSize
						case METHOD_EXIT_WITH_RETURN_VALUE =>
							val prefix = requestIdSize + threadIdSize + locationSize
							val valueSize = 1 + getValueSize(restData.drop(prefix).head)
							prefix + valueSize
						case MONITOR_CONTENDED_ENTER => requestIdSize + threadIdSize + taggedObjectId + locationSize
						case MONITOR_CONTENDED_ENTERED => requestIdSize + threadIdSize + taggedObjectId + locationSize
						case MONITOR_WAIT => requestIdSize + threadIdSize + taggedObjectId + locationSize + longSize
						case MONITOR_WAITED => requestIdSize + threadIdSize + taggedObjectId + locationSize + booleanSize
						case EXCEPTION => requestIdSize + threadIdSize + locationSize + taggedObjectId + locationSize
						case THREAD_START => requestIdSize + threadIdSize
						case THREAD_DEATH => requestIdSize + threadIdSize
						case CLASS_PREPARE =>
							val prefix = requestIdSize + threadIdSize + byteSize + idSizes.referenceTypeId
							val stringSize = intSize + restData.drop(prefix).take(intSize).toInt()
							prefix + stringSize + intSize
						case CLASS_UNLOAD =>
							val prefix = requestIdSize
							val stringSize = intSize + restData.drop(prefix).take(intSize).toInt()
							prefix + stringSize
						case FIELD_ACCESS => requestIdSize + threadIdSize + locationSize + byteSize + idSizes.referenceTypeId + idSizes.fieldId + taggedObjectId
						case FIELD_MODIFICATION =>
							val prefix = requestIdSize + threadIdSize + locationSize + byteSize + idSizes.referenceTypeId + idSizes.fieldId + taggedObjectId
							val stringSize = intSize + restData.drop(prefix).take(intSize).toInt()
							prefix + stringSize
						case VM_DEATH => requestIdSize
					})

					val compositeMessage = restData.take(length)
					val rest = restData.drop(length)

					val compositeRequestId = compositeMessage.take(requestIdSize).toInt()
					val compositeMessageRest = compositeMessage.drop(requestIdSize)

					val replacementRequestId = requestIdMap.get(compositeRequestId).getOrElse(compositeRequestId)

					ByteVector(dataType) ++ ByteVector.fromInt(replacementRequestId) ++ compositeMessageRest ++ replaceCompositeRequestIds(rest)
				}
			}

			private def getValueSize(tag: Byte): Int = {
				tag.toChar match {
					case '[' => idSizes.objectId // array object
					case 'B' => byteSize // byte value
					case 'C' => charSize // character value
					case 'L' => idSizes.objectId // object
					case 'F' => floatSize // float value
					case 'D' => doubleSize // double value
					case 'I' => intSize // int value
					case 'J' => longSize // long value
					case 'S' => shortSize // short value
					case 'V' => voidSize // void value
					case 'Z' => booleanSize // boolean value
					case 's' => idSizes.objectId // String object
					case 't' => idSizes.objectId // Thread object
					case 'g' => idSizes.objectId // ThreadGroup object
					case 'l' => idSizes.objectId // ClassLoader object
					case 'c' => idSizes.objectId // class object object
				}
			}

			// Event Kinds
			val SINGLE_STEP: Byte = 1
			val BREAKPOINT: Byte = 2
			val FRAME_POP: Byte = 3
			val EXCEPTION: Byte = 4
			val USER_DEFINED: Byte = 5
			val THREAD_START: Byte = 6
			val THREAD_DEATH: Byte = 7
			val THREAD_END: Byte = 7 //obsolete - was used in jvmdi
			val CLASS_PREPARE: Byte = 8
			val CLASS_UNLOAD: Byte = 9
			val CLASS_LOAD: Byte = 10
			val FIELD_ACCESS: Byte = 20
			val FIELD_MODIFICATION: Byte = 21
			val EXCEPTION_CATCH: Byte = 30
			val METHOD_ENTRY: Byte = 40
			val METHOD_EXIT: Byte = 41
			val METHOD_EXIT_WITH_RETURN_VALUE: Byte = 42
			val MONITOR_CONTENDED_ENTER: Byte = 43
			val MONITOR_CONTENDED_ENTERED: Byte = 44
			val MONITOR_WAIT: Byte = 45
			val MONITOR_WAITED: Byte = 46
			val VM_START: Byte = 90
			val VM_INIT: Byte = 90 //obsolete - was used in jvmd
			val VM_DEATH: Byte = 99
			val VM_DISCONNECTED: Byte = 100 //Never sent across JDWP
		}
	}
}
