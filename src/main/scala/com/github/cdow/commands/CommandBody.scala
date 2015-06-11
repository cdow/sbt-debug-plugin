package com.github.cdow.commands

import com.github.cdow.PrimitiveCodecs._
import com.github.cdow.responses.IdSizes
import scodec.{codecs, Codec}

object CommandBody {
	def compositeEvents(implicit idSizes: IdSizes): Codec[Seq[CompositeEvent]] =
		times(int,
			codecs.discriminated[CompositeEvent].by(byte)
				.\ (1) { case in: SingleStep => in } ((requestId :: threadID :: location).as[SingleStep])
				.\ (2) { case in: Breakpoint => in } ((requestId :: threadID :: location).as[Breakpoint])
				.\ (4) { case in: Exception => in } ((requestId :: threadID :: location :: taggedObjectID :: location).as[Exception])
				.\ (6) { case in: ThreadStart => in } ((requestId :: threadID).as[ThreadStart])
				.\ (7) { case in: ThreadDeath => in } ((requestId :: threadID).as[ThreadDeath])
				.\ (8) { case in: ClassPrepare => in } ((requestId :: threadID :: byte :: referenceTypeID :: string :: int).as[ClassPrepare])
				.\ (9) { case in: ClassUnload => in } ((requestId :: string).as[ClassUnload])
				.\ (20) { case in: FieldAccess => in } ((requestId :: threadID :: location :: byte :: referenceTypeID :: fieldID :: taggedObjectID).as[FieldAccess])
				.\ (21) { case in: FieldModification => in } ((requestId :: threadID :: location :: byte :: referenceTypeID :: fieldID :: taggedObjectID :: value).as[FieldModification])
				.\ (40) { case in: MethodEntry => in } ((requestId :: threadID :: location).as[MethodEntry])
				.\ (41) { case in: MethodExit => in } ((requestId :: threadID :: location).as[MethodExit])
				.\ (42) { case in: MethodExitWithReturnValue => in } ((requestId :: threadID :: location :: value).as[MethodExitWithReturnValue])
				.\ (43) { case in: MonitorContendedEnter => in } ((requestId :: threadID :: taggedObjectID :: location).as[MonitorContendedEnter])
				.\ (44) { case in: MonitorContendedEntered => in } ((requestId :: threadID :: taggedObjectID :: location).as[MonitorContendedEntered])
				.\ (45) { case in: MonitorWait => in } ((requestId :: threadID :: taggedObjectID :: location :: long).as[MonitorWait])
				.\ (46) { case in: MonitorWaited => in } ((requestId :: threadID :: taggedObjectID :: location :: boolean).as[MonitorWaited])
				.\ (90) { case in: VMStart => in } ((requestId :: threadID).as[VMStart])
				.\ (99) { case in: VMDeath => in } ((requestId.hlist).as[VMDeath])
		)

	def eventRequestModifiers(implicit idSizes: IdSizes): Codec[Seq[EventRequestModifiers]] =
		times(int,
			codecs.discriminated[EventRequestModifiers].by(byte)
				.\ (1) { case in: Count => in } (int.hlist.as[Count])
				.\ (2) { case in: Conditional => in } (int.hlist.as[Conditional])
				.\ (3) { case in: ThreadOnly => in } (threadID.hlist.as[ThreadOnly])
				.\ (4) { case in: ClassOnly => in } (referenceTypeID.hlist.as[ClassOnly])
				.\ (5) { case in: ClassMatch => in } (string.hlist.as[ClassMatch])
				.\ (6) { case in: ClassExclude => in } (string.hlist.as[ClassExclude])
				.\ (7) { case in: LocationOnly => in } (location.hlist.as[LocationOnly])
				.\ (8) { case in: ExceptionOnly => in } ((referenceTypeID :: boolean :: boolean).as[ExceptionOnly])
				.\ (9) { case in: FieldOnly => in } ((referenceTypeID :: fieldID).as[FieldOnly])
				.\ (10) { case in: Step => in } ((threadID :: int :: int).as[Step])
				.\ (11) { case in: InstanceOnly => in } (objectID.hlist.as[InstanceOnly])
				.\ (12) { case in: SourceNameMatch => in } (string.hlist.as[SourceNameMatch])
		)

	def classDefinitions(implicit idSizes: IdSizes): Codec[Seq[ClassDefinition]] =
		times(int, (referenceTypeID :: times(int, byte)).as[ClassDefinition])
}

sealed trait CompositeEvent
case class VMStart(requestID: RequestId, threadID: ThreadId) extends CompositeEvent
case class SingleStep(requestID: RequestId, threadID: ThreadId, location: Location) extends CompositeEvent
case class Breakpoint(requestID: RequestId, threadID: ThreadId, location: Location) extends CompositeEvent
case class MethodEntry(requestID: RequestId, threadID: ThreadId, location: Location) extends CompositeEvent
case class MethodExit(requestID: RequestId, threadID: ThreadId, location: Location) extends CompositeEvent
case class MethodExitWithReturnValue(requestID: RequestId, threadID: ThreadId, location: Location, value: Value) extends CompositeEvent
case class MonitorContendedEnter(requestID: RequestId, threadID: ThreadId, taggedObjectID: TaggedObjectId, location: Location) extends CompositeEvent
case class MonitorContendedEntered(requestID: RequestId, threadID: ThreadId, taggedObjectID: TaggedObjectId, location: Location) extends CompositeEvent
case class MonitorWait(requestID: RequestId, threadID: ThreadId, taggedObjectID: TaggedObjectId, location: Location, timeout: Long) extends CompositeEvent
case class MonitorWaited(requestID: RequestId, threadID: ThreadId, taggedObjectID: TaggedObjectId, location: Location, timedOut: Boolean) extends CompositeEvent
case class Exception(requestID: RequestId, threadID: ThreadId, throwLocation: Location, taggedObjectID: TaggedObjectId, catchLocation: Location) extends CompositeEvent
case class ThreadStart(requestID: RequestId, threadID: ThreadId) extends CompositeEvent
case class ThreadDeath(requestID: RequestId, threadID: ThreadId) extends CompositeEvent
case class ClassPrepare(requestID: RequestId, threadID: ThreadId, refTypeTag: Byte, referenceTypeID: ReferenceTypeId, typeSignature: String, status: Int) extends CompositeEvent
case class ClassUnload(requestID: RequestId, typeSignature: String) extends CompositeEvent
case class FieldAccess(requestID: RequestId, threadID: ThreadId, location: Location, refTypeTag: Byte, referenceTypeID: ReferenceTypeId, fieldID: FieldId, taggedObjectID: TaggedObjectId) extends CompositeEvent
case class FieldModification(requestID: RequestId, threadID: ThreadId, location: Location, refTypeTag: Byte, referenceTypeID: ReferenceTypeId, fieldID: FieldId, taggedObjectID: TaggedObjectId, value: Value) extends CompositeEvent
case class VMDeath(requestID: RequestId) extends CompositeEvent

sealed trait EventRequestModifiers
case class Count(count: Int) extends EventRequestModifiers
case class Conditional(exprId: Int) extends EventRequestModifiers
case class ThreadOnly(threadId: ThreadId) extends EventRequestModifiers
case class ClassOnly(referenceTypeID: ReferenceTypeId) extends EventRequestModifiers
case class ClassMatch(classPattern: String) extends EventRequestModifiers
case class ClassExclude(classPattern: String) extends EventRequestModifiers
case class LocationOnly(location: Location) extends EventRequestModifiers
case class ExceptionOnly(referenceTypeID: ReferenceTypeId, caught: Boolean, uncaught: Boolean) extends EventRequestModifiers
case class FieldOnly(referenceTypeID: ReferenceTypeId, fieldId: FieldId) extends EventRequestModifiers
case class Step(threadId: ThreadId, size: Int, depth: Int) extends EventRequestModifiers
case class InstanceOnly(objectId: ObjectId) extends EventRequestModifiers
case class SourceNameMatch(sourceNamePattern: String) extends EventRequestModifiers

case class ClassDefinition(referenceTypeID: ReferenceTypeId, bytes: Seq[Byte])