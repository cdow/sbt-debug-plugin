package com.github.cdow.commands

import com.github.cdow.PrimitiveCodecs._
import com.github.cdow.responses.IdSizes
import scodec.{codecs, Codec}

/**
 * Created by cdow on 12/28/14.
 */
object CommandBody {
	def compositeEvents(implicit idSizes: IdSizes): Codec[Seq[CompositeEvent]] =
		times(int,
			codecs.discriminated[CompositeEvent].by(byte)
				.\ (1) { case in: SingleStep => in } ((int :: threadID :: location).as[SingleStep])
				.\ (2) { case in: Breakpoint => in } ((int :: threadID :: location).as[Breakpoint])
				.\ (4) { case in: Exception => in } ((int :: threadID :: location :: taggedObjectID :: location).as[Exception])
				.\ (6) { case in: ThreadStart => in } ((int :: threadID).as[ThreadStart])
				.\ (7) { case in: ThreadDeath => in } ((int :: threadID).as[ThreadDeath])
				.\ (8) { case in: ClassPrepare => in } ((int :: threadID :: byte :: referenceTypeID :: string :: int).as[ClassPrepare])
				.\ (9) { case in: ClassUnload => in } ((int :: string).as[ClassUnload])
				.\ (20) { case in: FieldAccess => in } ((int :: threadID :: location :: byte :: referenceTypeID :: fieldID :: taggedObjectID).as[FieldAccess])
				.\ (21) { case in: FieldModification => in } ((int :: threadID :: location :: byte :: referenceTypeID :: fieldID :: taggedObjectID :: value).as[FieldModification])
				.\ (40) { case in: MethodEntry => in } ((int :: threadID :: location).as[MethodEntry])
				.\ (41) { case in: MethodExit => in } ((int :: threadID :: location).as[MethodExit])
				.\ (42) { case in: MethodExitWithReturnValue => in } ((int :: threadID :: location :: value).as[MethodExitWithReturnValue])
				.\ (43) { case in: MonitorContendedEnter => in } ((int :: threadID :: taggedObjectID :: location).as[MonitorContendedEnter])
				.\ (44) { case in: MonitorContendedEntered => in } ((int :: threadID :: taggedObjectID :: location).as[MonitorContendedEntered])
				.\ (45) { case in: MonitorWait => in } ((int :: threadID :: taggedObjectID :: location :: long).as[MonitorWait])
				.\ (46) { case in: MonitorWaited => in } ((int :: threadID :: taggedObjectID :: location :: boolean).as[MonitorWaited])
				.\ (90) { case in: VMStart => in } ((int :: threadID).as[VMStart])
				.\ (99) { case in: VMDeath => in } ((int.hlist).as[VMDeath])
		)
}

sealed trait CompositeEvent
case class VMStart(requestID: Int, threadID: ThreadId) extends CompositeEvent
case class SingleStep(requestID: Int, threadID: ThreadId, location: Location) extends CompositeEvent
case class Breakpoint(requestID: Int, threadID: ThreadId, location: Location) extends CompositeEvent
case class MethodEntry(requestID: Int, threadID: ThreadId, location: Location) extends CompositeEvent
case class MethodExit(requestID: Int, threadID: ThreadId, location: Location) extends CompositeEvent
case class MethodExitWithReturnValue(requestID: Int, threadID: ThreadId, location: Location, value: Value) extends CompositeEvent
case class MonitorContendedEnter(requestID: Int, threadID: ThreadId, taggedObjectID: TaggedObjectId, location: Location) extends CompositeEvent
case class MonitorContendedEntered(requestID: Int, threadID: ThreadId, taggedObjectID: TaggedObjectId, location: Location) extends CompositeEvent
case class MonitorWait(requestID: Int, threadID: ThreadId, taggedObjectID: TaggedObjectId, location: Location, timeout: Long) extends CompositeEvent
case class MonitorWaited(requestID: Int, threadID: ThreadId, taggedObjectID: TaggedObjectId, location: Location, timedOut: Boolean) extends CompositeEvent
case class Exception(requestID: Int, threadID: ThreadId, throwLocation: Location, taggedObjectID: TaggedObjectId, catchLocation: Location) extends CompositeEvent
case class ThreadStart(requestID: Int, threadID: ThreadId) extends CompositeEvent
case class ThreadDeath(requestID: Int, threadID: ThreadId) extends CompositeEvent
case class ClassPrepare(requestID: Int, threadID: ThreadId, refTypeTag: Byte, referenceTypeID: ReferenceTypeId, typeSignature: String, status: Int) extends CompositeEvent
case class ClassUnload(requestID: Int, typeSignature: String) extends CompositeEvent
case class FieldAccess(requestID: Int, threadID: ThreadId, location: Location, refTypeTag: Byte, referenceTypeID: ReferenceTypeId, fieldID: FieldId, taggedObjectID: TaggedObjectId) extends CompositeEvent
case class FieldModification(requestID: Int, threadID: ThreadId, location: Location, refTypeTag: Byte, referenceTypeID: ReferenceTypeId, fieldID: FieldId, taggedObjectID: TaggedObjectId, value: Value) extends CompositeEvent
case class VMDeath(requestID: Int) extends CompositeEvent

