package com.github.cdow

import com.github.cdow.responses.IdSizes
import scodec.{bits, DecodingContext, Codec, codecs}
import scodec.bits.{BitVector, ByteVector}
import scala.annotation.tailrec
import scalaz.{\/, \/-}
import shapeless.HNil

object PrimitiveCodecs {
	val byte: Codec[Byte] = codecs.bytes(1).xmap(_.get(0), { in => ByteVector(in) })
	val boolean: Codec[Boolean] = codecs.bool(8)
	val int: Codec[Int] = codecs.int(32)
	val long: Codec[Long] = codecs.long(64)

	sealed trait Value
	case class ArrayId(objectId: ObjectId) extends Value
	case class ByteValue(value: ByteVector) extends Value
	case class CharValue(value: ByteVector) extends Value
	case class ObjectId(id: ByteVector) extends Value
	case class FloatValue(value: ByteVector) extends Value
	case class DoubleValue(value: ByteVector) extends Value
	case class IntValue(value: ByteVector) extends Value
	case class LongValue(value: ByteVector) extends Value
	case class ShortValue(value: ByteVector) extends Value
	case object VoidValue extends Value
	case class BooleanValue(value: ByteVector) extends Value
	case class StringId(objectId: ObjectId) extends Value
	case class ThreadId(objectId: ObjectId) extends Value
	case class ThreadGroupId(objectId: ObjectId) extends Value
	case class ClassLoaderId(objectId: ObjectId) extends Value
	case class ClassObjectId(objectId: ObjectId) extends Value

	def objectID(implicit idSizes: IdSizes): Codec[ObjectId] = codecs.bytes(idSizes.objectId).xmap(ObjectId, _.id)
	def threadID(implicit idSizes: IdSizes): Codec[ThreadId] = objectID.xmap(ThreadId, _.objectId)
	def threadGroupID(implicit idSizes: IdSizes): Codec[ThreadGroupId] = objectID.xmap(ThreadGroupId, _.objectId)
	def stringID(implicit idSizes: IdSizes): Codec[StringId] = objectID.xmap(StringId, _.objectId)
	def classLoaderID(implicit idSizes: IdSizes): Codec[ClassLoaderId] = objectID.xmap(ClassLoaderId, _.objectId)
	def classObjectID(implicit idSizes: IdSizes): Codec[ClassObjectId] = objectID.xmap(ClassObjectId, _.objectId)
	def arrayID(implicit idSizes: IdSizes): Codec[ArrayId] = objectID.xmap(ArrayId, _.objectId)

	case class TaggedObjectId(objectType: Byte, objectId: ObjectId)
	def taggedObjectID(implicit idSizes: IdSizes): Codec[TaggedObjectId] =
		(byte :: objectID).as[TaggedObjectId]

	case class ReferenceTypeId(id: ByteVector)
	def referenceTypeID(implicit idSizes: IdSizes): Codec[ReferenceTypeId] =
		codecs.bytes(idSizes.referenceTypeId).xmap(ReferenceTypeId, _.id)

	case class ClassId(referenceTypeId: ReferenceTypeId)
	def classID(implicit idSizes: IdSizes): Codec[ClassId] = referenceTypeID.xmap(ClassId, _.referenceTypeId)

	case class InterfaceId(referenceTypeId: ReferenceTypeId)
	def interfaceID(implicit idSizes: IdSizes): Codec[InterfaceId] = referenceTypeID.xmap(InterfaceId, _.referenceTypeId)

	case class ArrayTypeId(referenceTypeId: ReferenceTypeId)
	def arrayTypeID(implicit idSizes: IdSizes): Codec[ArrayTypeId] = referenceTypeID.xmap(ArrayTypeId, _.referenceTypeId)

	case class MethodId(id: ByteVector)
	def methodID(implicit idSizes: IdSizes): Codec[MethodId] = codecs.bytes(idSizes.methodId).xmap(MethodId, _.id)

	case class FieldId(id: ByteVector)
	def fieldID(implicit idSizes: IdSizes): Codec[FieldId] = codecs.bytes(idSizes.fieldId).xmap(FieldId, _.id)

	case class FrameId(id: ByteVector)
	def frameID(implicit idSizes: IdSizes): Codec[FrameId] = codecs.bytes(idSizes.frameId).xmap(FrameId, _.id)

	case class Location(typeTag: Byte, classId: ClassId, methodId: MethodId, index: ByteVector)
	def location(implicit idSizes: IdSizes): Codec[Location] =
		(byte :: classID :: methodID :: codecs.bytes(8)).as[Location]

	val string = codecs.variableSizeBytes(int, codecs.utf8)

	def value(implicit idSizes: IdSizes): Codec[Value] =
		codecs.discriminated[Value].by(byte)
			.\ ('['.toByte) { case in :ArrayId => in } (arrayID)
			.\ ('B'.toByte) { case in :ByteValue => in } (codecs.bytes(1).xmap(ByteValue, _.value))
			.\ ('C'.toByte) { case in :CharValue => in } (codecs.bytes(2).xmap(CharValue, _.value))
			.\ ('L'.toByte) { case in :ObjectId => in } (objectID)
			.\ ('F'.toByte) { case in :FloatValue => in } (codecs.bytes(4).xmap(FloatValue, _.value))
			.\ ('D'.toByte) { case in :DoubleValue => in } (codecs.bytes(4).xmap(DoubleValue, _.value))
			.\ ('I'.toByte) { case in :IntValue => in } (codecs.bytes(4).xmap(IntValue, _.value))
			.\ ('J'.toByte) { case in :LongValue => in } (codecs.bytes(8).xmap(LongValue, _.value))
			.\ ('S'.toByte) { case in :ShortValue => in } (codecs.bytes(2).xmap(ShortValue, _.value))
			.\ ('V'.toByte) { case in @ VoidValue => in } (codecs.provide(VoidValue))
			.\ ('Z'.toByte) { case in :BooleanValue => in } (codecs.bytes(1).xmap(BooleanValue, _.value))
			.\ ('s'.toByte) { case in :StringId => in } (stringID)
			.\ ('t'.toByte) { case in :ThreadId => in } (threadID)
			.\ ('g'.toByte) { case in :ThreadGroupId => in } (threadGroupID)
			.\ ('l'.toByte) { case in :ClassLoaderId => in } (classLoaderID)
			.\ ('c'.toByte) { case in :ClassObjectId => in } (classObjectID)

	// TODO figure out how to parse untagged values
	//    untagged-value 	Variable 	A value as described above without the signature byte. This form is used when the signature information can be determined from context.
	//    arrayregion 	Variable 	A compact representation of values used with some array operations. The first byte is a signature byte which is used to identify the type. See JDWP.Tag for the possible values of this byte. Next is a four-byte integer indicating the number of values in the sequence. This is followed by the values themselves: Primitive values are encoded as a sequence of untagged-values; Object values are encoded as a sequence of values.

	val seqEmptyCodec: Codec[Seq[_]] = new Codec[Seq[_]] {
		def encode(hn: Seq[_]) = \/-(BitVector.empty)
		def decode(buffer: BitVector) = \/-((buffer, Seq.empty))
	}

	def times[A](times: Codec[Int], values: Codec[A]): Codec[Seq[A]] = new Codec[Seq[A]] {
		def encode(a: Seq[A]) = {
			val size = times.encode(a.size)
			val contentsSeq = a.map(values.encode)
			val contents = contentsSeq.reduce { (progress, value) =>
				for {
					encProgress <- progress
					encValue <- value
				} yield {
					encProgress ++ encValue
				}
			}

			for {
				s <- size
				c <- contents
			} yield { s ++ c }
		}

		def decode(b: BitVector) = {
			DecodingContext(times.decode).flatMap { size =>
				decodeStep(size, DecodingContext.liftE(\/-(Seq())))
			}.run(b)
		}

		@tailrec
		private def decodeStep(numSteps: Int, currentContext: DecodingContext[Seq[A]]): DecodingContext[Seq[A]] = {
			if (numSteps > 0) {
				val newContext = for {
					current <- currentContext
					next <- DecodingContext(values.decode)
				} yield {
					current :+ next
				}

				decodeStep(numSteps - 1, newContext)
			} else {
				currentContext
			}
		}
	}
}
