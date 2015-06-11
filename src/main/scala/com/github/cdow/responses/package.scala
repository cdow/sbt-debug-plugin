package com.github.cdow.responses

import com.github.cdow.PrimitiveCodecs._
import scodec.Codec
import scodec.bits.BitVector

case class VirtualMachineClassBySignature(refTypeTag: Byte, typeID: ReferenceTypeId, status: Int)
case class VirtualMachineAllClass(refTypeTag: Byte, typeID: ReferenceTypeId, signature: String, status: Int)
case class VirtualMachineAllClassWithGeneric(refTypeTag: Byte, typeID: ReferenceTypeId, signature: String, genericSignature: String, status: Int)

case class ReferenceTypeNestedType(refTypeTag: Byte, typeID: ReferenceTypeId)
case class ObjectReferenceReferenceType(refTypeTag: Byte, typeID: ReferenceTypeId)
case class ThreadReferenceFrame(frameID: FrameId, location: Location)
case class ClassLoaderReferenceVisibleClass(refTypeTag: Byte, typeID: ReferenceTypeId)
case class ClassObjectReferenceReflectedType(refTypeTag: Byte, typeID: ReferenceTypeId)

case class IdSizes(fieldId: Int, methodId: Int, objectId: Int, referenceTypeId: Int, frameId: Int)
case class EventRequestSet(requestID: RequestId)

object ResponseCodecs {
	implicit def virtualMachineClassesBySignature(implicit idSizes: IdSizes): Codec[Seq[VirtualMachineClassBySignature]] =
		times(int, (byte :: referenceTypeID :: int).as[VirtualMachineClassBySignature])
	implicit def virtualMachineAllClasses(implicit idSizes: IdSizes): Codec[Seq[VirtualMachineAllClass]] =
		times(int, (byte :: referenceTypeID :: string :: int).as[VirtualMachineAllClass])
	implicit def virtualMachineAllClassesWithGeneric(implicit idSizes: IdSizes): Codec[Seq[VirtualMachineAllClassWithGeneric]] =
		times(int, (byte :: referenceTypeID :: string :: string :: int).as[VirtualMachineAllClassWithGeneric])

	def decodeVirtualMachineClassesBySignature(binPacket: Array[Byte])(implicit idSizes: IdSizes): Seq[VirtualMachineClassBySignature] = {
		Codec.decodeValidValue[Seq[VirtualMachineClassBySignature]](BitVector(binPacket))
	}

	def encodeVirtualMachineClassesBySignature(response: Seq[VirtualMachineClassBySignature])(implicit idSizes: IdSizes): Array[Byte] = {
		Codec.encodeValid(response).toByteArray
	}

	def decodeVirtualMachineAllClass(binPacket: Array[Byte])(implicit idSizes: IdSizes): Seq[VirtualMachineAllClass] = {
		Codec.decodeValidValue[Seq[VirtualMachineAllClass]](BitVector(binPacket))
	}

	def encodeVirtualMachineAllClass(response: Seq[VirtualMachineAllClass])(implicit idSizes: IdSizes): Array[Byte] = {
		Codec.encodeValid(response).toByteArray
	}

	def decodeVirtualMachineAllClassWithGeneric(binPacket: Array[Byte])(implicit idSizes: IdSizes): Seq[VirtualMachineAllClassWithGeneric] = {
		Codec.decodeValidValue[Seq[VirtualMachineAllClassWithGeneric]](BitVector(binPacket))
	}

	def encodeVirtualMachineAllClassWithGeneric(response: Seq[VirtualMachineAllClassWithGeneric])(implicit idSizes: IdSizes): Array[Byte] = {
		Codec.encodeValid(response).toByteArray
	}

	implicit def referenceTypeNestedTypes(implicit idSizes: IdSizes): Codec[Seq[ReferenceTypeNestedType]] =
		times(int, (byte :: referenceTypeID).as[ReferenceTypeNestedType])
	implicit def referenceTypeInterfaces(implicit idSizes: IdSizes): Codec[Seq[InterfaceId]] =
		times(int, interfaceID)
	implicit def classTypeSuperClass(implicit idSizes: IdSizes): Codec[ClassId] =
		classID
	implicit def objectReferenceReferenceType(implicit idSizes: IdSizes): Codec[ObjectReferenceReferenceType] =
		(byte :: referenceTypeID).as[ObjectReferenceReferenceType]
	implicit def threadReferenceFrames(implicit idSizes: IdSizes): Codec[Seq[ThreadReferenceFrame]] =
		times(int, (frameID :: location).as[ThreadReferenceFrame])
	implicit def classLoaderReferenceVisibleClasses(implicit idSizes: IdSizes): Codec[Seq[ClassLoaderReferenceVisibleClass]] =
		times(int, (byte :: referenceTypeID).as[ClassLoaderReferenceVisibleClass])
	implicit def classObjectReferenceReflectedType(implicit idSizes: IdSizes): Codec[ClassObjectReferenceReflectedType] =
		(byte :: referenceTypeID).as[ClassObjectReferenceReflectedType]

	def decodeReferenceTypeNestedTypes(binPacket: Array[Byte])(implicit idSizes: IdSizes): Seq[ReferenceTypeNestedType] = {
		Codec.decodeValidValue[Seq[ReferenceTypeNestedType]](BitVector(binPacket))
	}

	def encodeReferenceTypeNestedTypes(response: Seq[ReferenceTypeNestedType])(implicit idSizes: IdSizes): Array[Byte] = {
		Codec.encodeValid(response).toByteArray
	}

	def decodeReferenceTypeInterfaces(binPacket: Array[Byte])(implicit idSizes: IdSizes): Seq[InterfaceId] = {
		Codec.decodeValidValue[Seq[InterfaceId]](BitVector(binPacket))
	}

	def encodeReferenceTypeInterfaces(response: Seq[InterfaceId])(implicit idSizes: IdSizes): Array[Byte] = {
		Codec.encodeValid(response).toByteArray
	}

	def decodeClassTypeSuperClass(binPacket: Array[Byte])(implicit idSizes: IdSizes): ClassId = {
		Codec.decodeValidValue[ClassId](BitVector(binPacket))
	}

	def encodeClassTypeSuperClass(response: ClassId)(implicit idSizes: IdSizes): Array[Byte] = {
		Codec.encodeValid(response).toByteArray
	}

	def decodeObjectReferenceReferenceType(binPacket: Array[Byte])(implicit idSizes: IdSizes): ObjectReferenceReferenceType = {
		Codec.decodeValidValue[ObjectReferenceReferenceType](BitVector(binPacket))
	}

	def encodeObjectReferenceReferenceType(response: ObjectReferenceReferenceType)(implicit idSizes: IdSizes): Array[Byte] = {
		Codec.encodeValid(response).toByteArray
	}

	def decodeThreadReferenceFrames(binPacket: Array[Byte])(implicit idSizes: IdSizes): Seq[ThreadReferenceFrame] = {
		Codec.decodeValidValue[Seq[ThreadReferenceFrame]](BitVector(binPacket))
	}

	def encodeThreadReferenceFrames(response: Seq[ThreadReferenceFrame])(implicit idSizes: IdSizes): Array[Byte] = {
		Codec.encodeValid(response).toByteArray
	}

	def decodeClassLoaderReferenceVisibleClassses(binPacket: Array[Byte])(implicit idSizes: IdSizes): Seq[ClassLoaderReferenceVisibleClass] = {
		Codec.decodeValidValue[Seq[ClassLoaderReferenceVisibleClass]](BitVector(binPacket))
	}

	def encodeClassLoaderReferenceVisibleClasses(response: Seq[ClassLoaderReferenceVisibleClass])(implicit idSizes: IdSizes): Array[Byte] = {
		Codec.encodeValid(response).toByteArray
	}

	def decodeClassObjectReferenceReflectedType(binPacket: Array[Byte])(implicit idSizes: IdSizes): ClassObjectReferenceReflectedType = {
		Codec.decodeValidValue[ClassObjectReferenceReflectedType](BitVector(binPacket))
	}

	def encodeClassObjectReferenceReflectedType(response: ClassObjectReferenceReflectedType)(implicit idSizes: IdSizes): Array[Byte] = {
		Codec.encodeValid(response).toByteArray
	}

	implicit val idSizes: Codec[IdSizes] = (int :: int :: int :: int :: int).as[IdSizes]
	implicit val eventRequestSet: Codec[EventRequestSet] = requestId.hlist.as[EventRequestSet]


	def decodeEventRequestSet(binPacket: Array[Byte]): EventRequestSet = {
		Codec.decodeValidValue[EventRequestSet](BitVector(binPacket))
	}

	def encodeEventRequestSet(setResponse: EventRequestSet): Array[Byte] = {
		Codec.encodeValid(setResponse).toByteArray
	}
}
