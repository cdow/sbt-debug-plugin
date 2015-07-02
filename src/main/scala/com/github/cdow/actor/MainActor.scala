package com.github.cdow.actor

import akka.actor.{FSM, Props}
import akka.util.ByteString
import com.github.cdow.PrimitiveCodecs._
import com.github.cdow._
import com.github.cdow.actor.debugger.DebuggerActor
import com.github.cdow.actor.vm.{VmMessage, VmActor}
import com.github.cdow.commands._
import com.github.cdow.responses.{EventRequestSet, IdSizes}
import com.github.cdow.responses.ResponseCodecs._
import sbt.Logger
import scodec.bits.ByteVector

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
	def props(debuggerPort: Int, vmPort: Int, logger: Logger) = Props(new MainActor(debuggerPort, vmPort, logger))
}

class MainActor(debuggerPort: Int, vmPort: Int, logger: Logger) extends FSM[MainState, Unit] {
	import MainState._

	// TODO determine this dynamically
	implicit val idSizes = IdSizes(8,8,8,8,8)

	val debuggerActor = context.actorOf(DebuggerActor.props(debuggerPort, self), "debugger")
	val vmActor = context.actorOf(VmActor.props(vmPort, self), "vm")

	var queuedMessages = Seq.empty[ByteString]
	// TODO distinguish between vm and debugger ids
	var awaitingReponse = Map.empty[Long, Command]
	val eventRequestManager = new EventRequestManager
	// TODO get idSizes dynamically
	val referenceTypeIdManager = new ReferenceTypeIdManager(idSizes)

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
			// TODO keep response from these from going to the debugger
			val newVmEvents = eventRequestManager.newVm()
			newVmEvents.foreach { newVmEvent =>
				val encodedEvent = ByteString(JdwpCodecs.encodePacket(newVmEvent))
				self ! encodedEvent
			}

			referenceTypeIdManager.newVm()

			queuedMessages.foreach { queuedMessage =>
				self ! queuedMessage
			}
			queuedMessages = Seq.empty
			goto(VmConnected)
		case Event(MainMessage.DebuggerDisconnected, ()) =>
			eventRequestManager.clearAllEvents()
			vmActor ! VmMessage.Disconnect
			goto(Idle)
	}

	when(VmConnected) {
		case Event(data: ByteString, ()) =>
			val decodedPackets = JdwpCodecs.decodePackets(data.toArray)

			decodedPackets.foreach { decoded =>
				val packetInfo = toPacketInfo(decoded)
				logger.debug("INITIAL:  " + packetInfo)

				awaitingReponse = packetInfo match {
					case CommandInfo(id, command) =>
						awaitingReponse + (id -> command)
					case ResponseInfo(id, _, _, _) =>
						awaitingReponse - id
				}

				packetInfo match {
					case CommandInfo(id, command) =>
						command match {
							case set: EventRequest.Set => eventRequestManager.newEventRequest(id, set)
							case EventRequest.Clear(eventKind, requestId) => eventRequestManager.clearEvent(requestId)
							case EventRequest.ClearAllBreakpoints => eventRequestManager.clearAllEvents()
							case _ => // do nothing
						}
					case ResponseInfo(id, _, data, command) =>
						command match {
							case EventRequest.Set(_, _, _) =>
								val decodedResponse = decodeEventRequestSet(data.toArray)
								val vmRequestId = decodedResponse.requestID
								eventRequestManager.eventRequestResponse(id, vmRequestId)
							case _ => // do nothing
						}
				}

				if (self == sender() || debuggerActor == sender()) {
					val requestIdsConverted = convertToVmRequestIds(packetInfo)
					val result = convertToVmReferenceTypeIds(requestIdsConverted)

					logger.debug("DEBUGGER: " + result)
					vmActor ! ByteString(JdwpCodecs.encodePacket(result.toJdwpPacket))
				} else {
					if (!isVmDeathEvent(decoded)) {
						val requestIdsConverted = convertToDebuggerRequestIds(packetInfo)
						val result = convertToDebuggerReferenceTypeIds(requestIdsConverted)

						logger.debug("VM:       " + result)
						debuggerActor ! ByteString(JdwpCodecs.encodePacket(result.toJdwpPacket))
					}
				}

				// TODO verify that input matches output aside from transormations
//				val finalData = ByteString(JdwpCodecs.encodePacket(packetInfo.toJdwpPacket))
//				if (data != finalData) {
//					throw new RuntimeException(s"difference for $packetInfo, expected: $data, got: $finalData")
//				}
			}

			stay
		case Event(MainMessage.VmDisconnected, ()) =>
			// TODO handle case where there are still messages awaitingResponse, effects EventRequestManager
			goto(DebuggerConnected)
		case Event(MainMessage.DebuggerDisconnected, ()) =>
			eventRequestManager.clearAllEvents()
			vmActor ! VmMessage.Disconnect
			goto(Idle)
	}

	// TODO report all classes as unloaded on VmDeath
	private def isVmDeathEvent(message: JdwpPacket): Boolean = {
		message.message match {
			case CommandPacket(commands.Event.Composite(_, events)) =>
				events.contains(VMDeath(RequestId(0)))
			case _ => false
		}
	}

	trait PacketInfo {
		def toJdwpPacket: JdwpPacket
	}
	case class ResponseInfo(id: Long, errorCode: Int, data: ByteVector, originalCommand: Command) extends PacketInfo {
		override def toJdwpPacket: JdwpPacket = JdwpPacket(id, ResponsePacket(errorCode, data))
	}
	case class CommandInfo(id: Long, command: Command) extends PacketInfo {
		override def toJdwpPacket: JdwpPacket = JdwpPacket(id, CommandPacket(command))
	}

	def toPacketInfo(packet: JdwpPacket): PacketInfo = packet match {
		case JdwpPacket(id, cp :CommandPacket) => CommandInfo(id, cp.command)
		case JdwpPacket(id, rp: ResponsePacket) =>
			val command = awaitingReponse(id)
			ResponseInfo(id, rp.errorCode, rp.data, command)
	}

	// These requestId conversions would be so much easier if the compiler didn't choke on shapeless.everything
	private val messageLens = shapeless.lens[JdwpPacket] >> 'message
	private val commandLens = shapeless.lens[CommandInfo] >> 'command
	private val compositeLens = shapeless.lens[commands.Event.Composite] >> 'events
	private val responseInfoDataLens = shapeless.lens[ResponseInfo] >> 'data
	private def convertToDebuggerRequestIds(packetInfo: PacketInfo): PacketInfo = {
		val eventRequestSetDataLens = shapeless.lens[EventRequestSet] >> 'requestID
		packetInfo match {
			case ci: CommandInfo =>
				commandLens.modify(ci) {
					case ec: commands.Event.Composite =>
						compositeLens.modify(ec)(_.map {
							case event: VMStart => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: SingleStep => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: Breakpoint => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: MethodEntry => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: MethodExit => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: MethodExitWithReturnValue => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: MonitorContendedEnter => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: MonitorContendedEntered => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: MonitorWait => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: MonitorWaited => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: commands.Exception => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: ThreadStart => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: commands.ThreadDeath => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: ClassPrepare => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: ClassUnload => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: FieldAccess => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: FieldModification => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
							case event: VMDeath => event.copy(requestID = eventRequestManager.toDebuggerId(event.requestID))
						})
					case other => other
				}
			case ri: ResponseInfo =>
				ri.originalCommand match {
					case _: EventRequest.Set =>
						responseInfoDataLens.modify(ri) { data =>
							val parsedData = decodeEventRequestSet(data.toArray)
							val modifiedParsedData = eventRequestSetDataLens.modify(parsedData)(eventRequestManager.toDebuggerId)
							ByteVector(encodeEventRequestSet(modifiedParsedData))
						}
					case _ => ri
				}
		}
	}

	private def convertToVmRequestIds(packetInfo: PacketInfo): PacketInfo = {
		val clearLens = shapeless.lens[EventRequest.Clear] >> 'requestId
		packetInfo match {
			case ci: CommandInfo =>
				commandLens.modify(ci) {
					case erc: EventRequest.Clear => clearLens.modify(erc) { requestId =>
						// If we don't have an active event for the requestId, mark it as a system
						// event (requestId 0) so the clear is ignored.
						// TODO make sure this is a good way to handle this
						eventRequestManager.toVmId(requestId).getOrElse(RequestId(0))
					}
					case other => other
				}
			case ri: ResponseInfo => ri
		}
	}

	val classIdLens = shapeless.lens[ClassId] >> 'referenceTypeId
	// TODO make use of classIdLens in this lens
	val locationLens = shapeless.lens[Location] >> 'classId >> 'referenceTypeId

	private def convertToDebuggerReferenceTypeIds(packetInfo: PacketInfo): PacketInfo = {
		val interfaceIdLens = shapeless.lens[InterfaceId] >> 'referenceTypeId

		packetInfo match {
			case ci: CommandInfo =>
				commandLens.modify(ci) {
					case ec: commands.Event.Composite =>
						compositeLens.modify(ec)(_.map {
							case event: SingleStep => event.copy(location = locationLens.modify(event.location) (referenceTypeIdManager.toDebuggerId))
							case event: Breakpoint => event.copy(location = locationLens.modify(event.location) (referenceTypeIdManager.toDebuggerId))
							case event: MethodEntry => event.copy(location = locationLens.modify(event.location) (referenceTypeIdManager.toDebuggerId))
							case event: MethodExit => event.copy(location = locationLens.modify(event.location) (referenceTypeIdManager.toDebuggerId))
							case event: MethodExitWithReturnValue => event.copy(location = locationLens.modify(event.location) (referenceTypeIdManager.toDebuggerId))
							case event: MonitorContendedEnter => event.copy(location = locationLens.modify(event.location) (referenceTypeIdManager.toDebuggerId))
							case event: MonitorContendedEntered => event.copy(location = locationLens.modify(event.location) (referenceTypeIdManager.toDebuggerId))
							case event: MonitorWait => event.copy(location = locationLens.modify(event.location) (referenceTypeIdManager.toDebuggerId))
							case event: MonitorWaited => event.copy(location = locationLens.modify(event.location) (referenceTypeIdManager.toDebuggerId))
							case event: Exception => event.copy(
								throwLocation = locationLens.modify(event.throwLocation) (referenceTypeIdManager.toDebuggerId),
								catchLocation = locationLens.modify(event.catchLocation) (referenceTypeIdManager.toDebuggerId))
							case event: ClassPrepare => event.copy(referenceTypeID = referenceTypeIdManager.toDebuggerId(event.referenceTypeID))
							case event: FieldAccess => event.copy(
								referenceTypeID = referenceTypeIdManager.toDebuggerId(event.referenceTypeID),
								location = locationLens.modify(event.location) (referenceTypeIdManager.toDebuggerId))
							case event: FieldModification => event.copy(
								referenceTypeID = referenceTypeIdManager.toDebuggerId(event.referenceTypeID),
								location = locationLens.modify(event.location) (referenceTypeIdManager.toDebuggerId))
							case other => other
						})
					case other => other
				}
			case ri: ResponseInfo =>
				ri.originalCommand match {
					case _: VirtualMachine.ClassesBySignature =>
						responseInfoDataLens.modify(ri) { data =>
							val parsedData = decodeVirtualMachineClassesBySignature(data.toArray)
							val modifiedParsedData = parsedData.map { originalParsedData =>
								originalParsedData.copy(typeID = referenceTypeIdManager.toDebuggerId(originalParsedData.typeID))
							}
							ByteVector(encodeVirtualMachineClassesBySignature(modifiedParsedData))
						}
					case _: VirtualMachine.AllClasses =>
						responseInfoDataLens.modify(ri) { data =>
							val parsedData = decodeVirtualMachineAllClass(data.toArray)
							val modifiedParsedData = parsedData.map { originalParsedData =>
								originalParsedData.copy(typeID = referenceTypeIdManager.toDebuggerId(originalParsedData.typeID))
							}
							ByteVector(encodeVirtualMachineAllClass(modifiedParsedData))
						}
					case _: VirtualMachine.AllClassesWithGeneric =>
						responseInfoDataLens.modify(ri) { data =>
							val parsedData = decodeVirtualMachineAllClassWithGeneric(data.toArray)
							val modifiedParsedData = parsedData.map { originalParsedData =>
								originalParsedData.copy(typeID = referenceTypeIdManager.toDebuggerId(originalParsedData.typeID))
							}
							ByteVector(encodeVirtualMachineAllClassWithGeneric(modifiedParsedData))
						}
					case _: ReferenceType.NestedTypes =>
						responseInfoDataLens.modify(ri) { data =>
							val parsedData = decodeReferenceTypeNestedTypes(data.toArray)
							val modifiedParsedData = parsedData.map { originalParsedData =>
								originalParsedData.copy(typeID = referenceTypeIdManager.toDebuggerId(originalParsedData.typeID))
							}
							ByteVector(encodeReferenceTypeNestedTypes(modifiedParsedData))
						}
					case _: ReferenceType.Interfaces =>
						responseInfoDataLens.modify(ri) { data =>
							val parsedData = decodeReferenceTypeInterfaces(data.toArray)
							val modifiedParsedData = parsedData.map { originalParsedData =>
								interfaceIdLens.modify(originalParsedData)(referenceTypeIdManager.toDebuggerId)
							}
							ByteVector(encodeReferenceTypeInterfaces(modifiedParsedData))
						}
					case _: ClassType.Superclass =>
						responseInfoDataLens.modify(ri) { data =>
							val parsedData = decodeClassTypeSuperClass(data.toArray)
							val modifiedParsedData = classIdLens.modify(parsedData)(referenceTypeIdManager.toDebuggerId)
							ByteVector(encodeClassTypeSuperClass(modifiedParsedData))
						}
					case _: ObjectReference.ReferenceType =>
						responseInfoDataLens.modify(ri) { data =>
							val parsedData = decodeObjectReferenceReferenceType(data.toArray)
							val modifiedParsedData = parsedData.copy(typeID = referenceTypeIdManager.toDebuggerId(parsedData.typeID))
							ByteVector(encodeObjectReferenceReferenceType(modifiedParsedData))
						}
					case _: ThreadReference.Frames =>
						responseInfoDataLens.modify(ri) { data =>
							val parsedData = decodeThreadReferenceFrames(data.toArray)
							val modifiedParsedData = parsedData.map { frame =>
								frame.copy(location = locationLens.modify(frame.location)(referenceTypeIdManager.toDebuggerId))
							}
							ByteVector(encodeThreadReferenceFrames(modifiedParsedData))
						}
					case _: ClassLoaderReference.VisibleClasses =>
						responseInfoDataLens.modify(ri) { data =>
							val parsedData = decodeClassLoaderReferenceVisibleClassses(data.toArray)
							val modifiedParsedData = parsedData.map { originalParsedData =>
								originalParsedData.copy(typeID = referenceTypeIdManager.toDebuggerId(originalParsedData.typeID))
							}
							ByteVector(encodeClassLoaderReferenceVisibleClasses(modifiedParsedData))
						}
					case _: ClassObjectReference.ReflectedType =>
						responseInfoDataLens.modify(ri) { data =>
							val parsedData = decodeClassObjectReferenceReflectedType(data.toArray)
							val modifiedParsedData = parsedData.copy(typeID = referenceTypeIdManager.toDebuggerId(parsedData.typeID))
							ByteVector(encodeClassObjectReferenceReflectedType(modifiedParsedData))
						}
					case _ => ri
				}
			}
	}

	private def convertToVmReferenceTypeIds(packetInfo: PacketInfo): PacketInfo = {
		val redefineClassesLens = shapeless.lens[VirtualMachine.RedefineClasses] >> 'classes
		val eventRequestSetModifiersLens = shapeless.lens[EventRequest.Set] >> 'modifiers
		val arrayTypeIdLens = shapeless.lens[ArrayTypeId] >> 'referenceTypeId

		packetInfo match {
			case ci: CommandInfo =>
				commandLens.modify(ci) {
					case command: VirtualMachine.RedefineClasses => redefineClassesLens.modify(command)(_.map { classDefinition =>
						classDefinition.copy(referenceTypeID = referenceTypeIdManager.toVmId(classDefinition.referenceTypeID))
					})
					case command: VirtualMachine.InstanceCounts => command.copy(referenceTypeIDs = command.referenceTypeIDs.map(referenceTypeIdManager.toVmId))
					case command: ReferenceType.Signature => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.ClassLoader => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.Modifiers => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.Fields => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.Methods => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.GetValues => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.SourceFile => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.NestedTypes => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.Status => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.Interfaces => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.ClassObject => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.SourceDebugExtension => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.SignatureWithGeneric => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.FieldsWithGeneric => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.MethodsWithGeneric => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.Instances => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.ClassFileVersion => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ReferenceType.ConstantPool => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: ClassType.Superclass => command.copy(clazz = classIdLens.modify(command.clazz)(referenceTypeIdManager.toVmId))
					case command: ClassType.SetValues => command.copy(clazz = classIdLens.modify(command.clazz)(referenceTypeIdManager.toVmId))
					case command: ClassType.InvokeMethod => command.copy(clazz = classIdLens.modify(command.clazz)(referenceTypeIdManager.toVmId))
					case command: ClassType.NewInstance => command.copy(clazz = classIdLens.modify(command.clazz)(referenceTypeIdManager.toVmId))
					case command: ArrayType.NewInstance => command.copy(arrayTypeID = arrayTypeIdLens.modify(command.arrayTypeID)(referenceTypeIdManager.toVmId))
					case command: Method.LineTable => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: Method.VariableTable => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: Method.Bytecodes => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: Method.IsObsolete => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: Method.VariableTableWithGeneric => command.copy(referenceTypeID = referenceTypeIdManager.toVmId(command.referenceTypeID))
					case command: EventRequest.Set => eventRequestSetModifiersLens.modify(command) (_.map {
						case modifier: ClassOnly => modifier.copy(referenceTypeID = referenceTypeIdManager.toVmId(modifier.referenceTypeID))
						case modifier: LocationOnly => modifier.copy(location = locationLens.modify(modifier.location) (referenceTypeIdManager.toVmId))
						case modifier: ExceptionOnly => modifier.copy(referenceTypeID = referenceTypeIdManager.toVmId(modifier.referenceTypeID))
						case modifier: FieldOnly => modifier.copy(referenceTypeID = referenceTypeIdManager.toVmId(modifier.referenceTypeID))
						case other => other
					})
					case other => other
				}
			case ri: ResponseInfo => ri
		}
	}
}
