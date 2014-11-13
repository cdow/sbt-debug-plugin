package com.github.cdow

import com.github.cdow.responses.IdSizes
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs

object PrimitiveCodecs {
	val byte: Codec[Byte] = codecs.bytes(1).xmap(_.get(0), { in => ByteVector(in) })
	val boolean: Codec[Boolean] = codecs.bool(8)
	val int: Codec[Int] = codecs.int(32)
	val long: Codec[Long] = codecs.long(64)

	case class ObjectId(id: ByteVector)
	def objectID(implicit idSizes: IdSizes): Codec[ObjectId] = codecs.bytes(idSizes.objectId).as[ObjectId]

	case class TaggedObjectId(objectType: Byte, objectId: ObjectId)
	def taggedObjectID(implicit idSizes: IdSizes): Codec[TaggedObjectId] =
		(byte :: objectID).as[TaggedObjectId]

	case class ThreadId(objectId: ObjectId)
	def threadID(implicit idSizes: IdSizes): Codec[ThreadId] = objectID.as[ThreadId]

	case class ThreadGroupId(objectId: ObjectId)
	def threadGroupID(implicit idSizes: IdSizes): Codec[ThreadGroupId] = objectID.as[ThreadGroupId]

	case class StringId(objectId: ObjectId)
	def stringID(implicit idSizes: IdSizes): Codec[StringId] = objectID.as[StringId]

	case class ClassLoaderId(objectId: ObjectId)
	def classLoaderID(implicit idSizes: IdSizes): Codec[ClassLoaderId] = objectID.as[ClassLoaderId]

	case class ClassObjectId(objectId: ObjectId)
	def classObjectID(implicit idSizes: IdSizes): Codec[ClassObjectId] = objectID.as[ClassObjectId]

	case class ArrayId(objectId: ObjectId)
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

	// TODO
	//    value 	Variable 	A value retrieved from the target VM. The first byte is a signature byte which is used to identify the type. See JDWP.Tag for the possible values of this byte. It is followed immediately by the value itself. This value can be an objectID (see Get ID Sizes) or a primitive value (1 to 8 bytes). More details about each value type can be found in the next table.
	//    untagged-value 	Variable 	A value as described above without the signature byte. This form is used when the signature information can be determined from context.
	//    arrayregion 	Variable 	A compact representation of values used with some array operations. The first byte is a signature byte which is used to identify the type. See JDWP.Tag for the possible values of this byte. Next is a four-byte integer indicating the number of values in the sequence. This is followed by the values themselves: Primitive values are encoded as a sequence of untagged-values; Object values are encoded as a sequence of values.
}
