package com.github.cdow

import com.github.cdow.responses.IdSizes
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs

object PrimitiveCodecs {
	sealed trait Value
	case class ArrayId(objectId: ObjectId) extends Value
	case class ByteValue(value: ByteValue) extends Value
	case class CharValue(value: ByteValue) extends Value
	case class ObjectId(id: ByteVector) extends Value
	case class FloatValue(value: ByteValue) extends Value
	case class DoubleValue(value: ByteValue) extends Value
	case class IntValue(value: ByteValue) extends Value
	case class LongValue(value: ByteValue) extends Value
	case class ShortValue(value: ByteValue) extends Value
	case object VoidValue extends Value
	case class BooleanValue(value: ByteValue) extends Value
	case class StringId(objectId: ObjectId) extends Value
	case class ThreadId(objectId: ObjectId) extends Value
	case class ThreadGroupId(objectId: ObjectId) extends Value
	case class ClassLoaderId(objectId: ObjectId) extends Value
	case class ClassObjectId(objectId: ObjectId) extends Value

	val byte: Codec[Byte] = codecs.bytes(1).xmap(_.get(0), { in => ByteVector(in) })
	val boolean: Codec[Boolean] = codecs.bool(8)
	val int: Codec[Int] = codecs.int(32)
	val long: Codec[Long] = codecs.long(64)

	def objectID(implicit idSizes: IdSizes): Codec[ObjectId] = codecs.bytes(idSizes.objectId).as[ObjectId]

	case class TaggedObjectId(objectType: Byte, objectId: ObjectId)
	def taggedObjectID(implicit idSizes: IdSizes): Codec[TaggedObjectId] =
		(byte :: objectID).as[TaggedObjectId]

	def threadID(implicit idSizes: IdSizes): Codec[ThreadId] = objectID.as[ThreadId]

	def threadGroupID(implicit idSizes: IdSizes): Codec[ThreadGroupId] = objectID.as[ThreadGroupId]

	def stringID(implicit idSizes: IdSizes): Codec[StringId] = objectID.as[StringId]

	def classLoaderID(implicit idSizes: IdSizes): Codec[ClassLoaderId] = objectID.as[ClassLoaderId]

	def classObjectID(implicit idSizes: IdSizes): Codec[ClassObjectId] = objectID.as[ClassObjectId]

	def arrayID(implicit idSizes: IdSizes): Codec[ArrayId] = objectID.as[ArrayId]

	case class ReferenceTypeId(id: ByteVector)
	def referenceTypeID(implicit idSizes: IdSizes): Codec[ReferenceTypeId] =
		codecs.bytes(idSizes.referenceTypeId).as[ReferenceTypeId]

	case class ClassId(referenceTypeId: ReferenceTypeId)
	def classID(implicit idSizes: IdSizes): Codec[ClassId] = referenceTypeID.as[ClassId]

	case class InterfaceId(referenceTypeId: ReferenceTypeId)
	def interfaceID(implicit idSizes: IdSizes): Codec[InterfaceId] = referenceTypeID.as[InterfaceId]

	case class ArrayTypeId(referenceTypeId: ReferenceTypeId)
	def arrayTypeID(implicit idSizes: IdSizes): Codec[ArrayTypeId] = referenceTypeID.as[ArrayTypeId]

	case class MethodId(id: ByteVector)
	def methodID(implicit idSizes: IdSizes): Codec[MethodId] = codecs.bytes(idSizes.methodId).as[MethodId]

	case class FieldId(id: ByteVector)
	def fieldID(implicit idSizes: IdSizes): Codec[FieldId] = codecs.bytes(idSizes.fieldId).as[FieldId]

	case class FrameId(id: ByteVector)
	def frameID(implicit idSizes: IdSizes): Codec[FrameId] = codecs.bytes(idSizes.frameId).as[FrameId]

	case class Location(typeTag: Byte, classId: ClassId, methodId: MethodId, index: ByteVector)
	def location(implicit idSizes: IdSizes): Codec[Location] =
		(byte :: classID :: methodID :: codecs.bytes(8)).as[Location]

	val string = codecs.variableSizeBytes(int, codecs.utf8)

	def value(implicit idSizes: IdSizes): Codec[Value] =
		codecs.discriminated[Value].by(byte)
			.\ ('['.toByte) { case in :ArrayId => in } (arrayID)
			.\ ('B'.toByte) { case in :ByteValue => in } (codecs.bytes(1).as[ByteValue])
			.\ ('C'.toByte) { case in :CharValue => in } (codecs.bytes(2).as[CharValue])
			.\ ('L'.toByte) { case in :ObjectId => in } (objectID)
			.\ ('F'.toByte) { case in :FloatValue => in } (codecs.bytes(4).as[FloatValue])
			.\ ('D'.toByte) { case in :DoubleValue => in } (codecs.bytes(4).as[DoubleValue])
			.\ ('I'.toByte) { case in :IntValue => in } (codecs.bytes(4).as[IntValue])
			.\ ('J'.toByte) { case in :LongValue => in } (codecs.bytes(8).as[LongValue])
			.\ ('S'.toByte) { case in :ShortValue => in } (codecs.bytes(2).as[ShortValue])
			.\ ('V'.toByte) { case in @ VoidValue => in } (codecs.provide(VoidValue))
			.\ ('Z'.toByte) { case in :BooleanValue => in } (codecs.bytes(1).as[BooleanValue])
			.\ ('s'.toByte) { case in :StringId => in } (stringID)
			.\ ('t'.toByte) { case in :ThreadId => in } (threadID)
			.\ ('g'.toByte) { case in :ThreadGroupId => in } (threadGroupID)
			.\ ('l'.toByte) { case in :ClassLoaderId => in } (classLoaderID)
			.\ ('c'.toByte) { case in :ClassObjectId => in } (classObjectID)

	// TODO figure out how to parse untagged values
	//    untagged-value 	Variable 	A value as described above without the signature byte. This form is used when the signature information can be determined from context.
	//    arrayregion 	Variable 	A compact representation of values used with some array operations. The first byte is a signature byte which is used to identify the type. See JDWP.Tag for the possible values of this byte. Next is a four-byte integer indicating the number of values in the sequence. This is followed by the values themselves: Primitive values are encoded as a sequence of untagged-values; Object values are encoded as a sequence of values.
}
