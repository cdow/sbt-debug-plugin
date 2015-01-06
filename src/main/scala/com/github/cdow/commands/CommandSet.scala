package com.github.cdow.commands

import com.github.cdow.PrimitiveCodecs.byte
import com.github.cdow.PrimitiveCodecs.int
import com.github.cdow.responses.IdSizes

import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs

sealed trait CommandSet
sealed trait Command

sealed trait VirtualMachineCommand extends Command
object VirtualMachine extends CommandSet {
	case class Version(data: ByteVector) extends VirtualMachineCommand
	case class ClassesBySignature(data: ByteVector) extends VirtualMachineCommand
	case class AllClasses(data: ByteVector) extends VirtualMachineCommand
	case class AllThreads(data: ByteVector) extends VirtualMachineCommand
	case class TopLevelThreadGroups(data: ByteVector) extends VirtualMachineCommand
	case class Dispose(data: ByteVector) extends VirtualMachineCommand
	case class IDSizes(data: ByteVector) extends VirtualMachineCommand
	case class Suspend(data: ByteVector) extends VirtualMachineCommand
	case class Resume(data: ByteVector) extends VirtualMachineCommand
	case class Exit(data: ByteVector) extends VirtualMachineCommand
	case class CreateString(data: ByteVector) extends VirtualMachineCommand
	case class Capabilities(data: ByteVector) extends VirtualMachineCommand
	case class ClassPaths(data: ByteVector) extends VirtualMachineCommand
	case class DisposeObjects(data: ByteVector) extends VirtualMachineCommand
	case class HoldEvents(data: ByteVector) extends VirtualMachineCommand
	case class ReleaseEvents(data: ByteVector) extends VirtualMachineCommand
	case class CapabilitiesNew(data: ByteVector) extends VirtualMachineCommand
	case class RedefineClasses(data: ByteVector) extends VirtualMachineCommand
	case class SetDefaultStratum(data: ByteVector) extends VirtualMachineCommand
	case class AllClassesWithGeneric(data: ByteVector) extends VirtualMachineCommand
	case class InstanceCounts(data: ByteVector) extends VirtualMachineCommand
}

sealed trait ReferenceTypeCommand extends Command
case object ReferenceType extends CommandSet {
	case class Signature(data: ByteVector) extends ReferenceTypeCommand
	case class ClassLoader(data: ByteVector) extends ReferenceTypeCommand
	case class Modifiers(data: ByteVector) extends ReferenceTypeCommand
	case class Fields(data: ByteVector) extends ReferenceTypeCommand
	case class Methods(data: ByteVector) extends ReferenceTypeCommand
	case class GetValues(data: ByteVector) extends ReferenceTypeCommand
	case class SourceFile(data: ByteVector) extends ReferenceTypeCommand
	case class NestedTypes(data: ByteVector) extends ReferenceTypeCommand
	case class Status(data: ByteVector) extends ReferenceTypeCommand
	case class Interfaces(data: ByteVector) extends ReferenceTypeCommand
	case class ClassObject(data: ByteVector) extends ReferenceTypeCommand
	case class SourceDebugExtension(data: ByteVector) extends ReferenceTypeCommand
	case class SignatureWithGeneric(data: ByteVector) extends ReferenceTypeCommand
	case class FieldsWithGeneric(data: ByteVector) extends ReferenceTypeCommand
	case class MethodsWithGeneric(data: ByteVector) extends ReferenceTypeCommand
	case class Instances(data: ByteVector) extends ReferenceTypeCommand
	case class ClassFileVersion(data: ByteVector) extends ReferenceTypeCommand
	case class ConstantPool(data: ByteVector) extends ReferenceTypeCommand
}

sealed trait ClassTypeCommand extends Command
case object ClassType extends CommandSet {
	case class Superclass(data: ByteVector) extends ClassTypeCommand
	case class SetValues(data: ByteVector) extends ClassTypeCommand
	case class InvokeMethod(data: ByteVector) extends ClassTypeCommand
	case class NewInstance(data: ByteVector) extends ClassTypeCommand
}

sealed trait ArrayTypeCommand extends Command
case object ArrayType extends CommandSet {
	case class NewInstance(data: ByteVector) extends ArrayTypeCommand
}

sealed trait InterfaceTypeCommand extends Command
case object InterfaceType extends CommandSet

sealed trait MethodCommand extends Command
case object Method extends CommandSet {
	case class LineTable(data: ByteVector) extends MethodCommand
	case class VariableTable(data: ByteVector) extends MethodCommand
	case class Bytecodes(data: ByteVector) extends MethodCommand
	case class IsObsolete(data: ByteVector) extends MethodCommand
	case class VariableTableWithGeneric(data: ByteVector) extends MethodCommand
}

sealed trait FieldCommand extends Command
case object Field extends CommandSet

sealed trait ObjectReferenceCommand extends Command
case object ObjectReference extends CommandSet {
	case class ReferenceType(data: ByteVector) extends ObjectReferenceCommand
	case class GetValues(data: ByteVector) extends ObjectReferenceCommand
	case class SetValues(data: ByteVector) extends ObjectReferenceCommand
	case class MonitorInfo(data: ByteVector) extends ObjectReferenceCommand
	case class InvokeMethod(data: ByteVector) extends ObjectReferenceCommand
	case class DisableCollection(data: ByteVector) extends ObjectReferenceCommand
	case class EnableCollection(data: ByteVector) extends ObjectReferenceCommand
	case class IsCollected(data: ByteVector) extends ObjectReferenceCommand
	case class ReferringObjects(data: ByteVector) extends ObjectReferenceCommand
}

sealed trait StringReferenceCommand extends Command
case object StringReference extends CommandSet {
	case class Value(data: ByteVector) extends StringReferenceCommand
}

sealed trait ThreadReferenceCommand extends Command
case object ThreadReference extends CommandSet {
	case class Name(data: ByteVector) extends ThreadReferenceCommand
	case class Suspend(data: ByteVector) extends ThreadReferenceCommand
	case class Resume(data: ByteVector) extends ThreadReferenceCommand
	case class Status(data: ByteVector) extends ThreadReferenceCommand
	case class ThreadGroup(data: ByteVector) extends ThreadReferenceCommand
	case class Frames(data: ByteVector) extends ThreadReferenceCommand
	case class FrameCount(data: ByteVector) extends ThreadReferenceCommand
	case class OwnedMonitors(data: ByteVector) extends ThreadReferenceCommand
	case class CurrentContendedMonitor(data: ByteVector) extends ThreadReferenceCommand
	case class Stop(data: ByteVector) extends ThreadReferenceCommand
	case class Interrupt(data: ByteVector) extends ThreadReferenceCommand
	case class SuspendCount(data: ByteVector) extends ThreadReferenceCommand
	case class OwnedMonitorsStackDepthInfo(data: ByteVector) extends ThreadReferenceCommand
	case class ForceEarlyReturn(data: ByteVector) extends ThreadReferenceCommand
}

sealed trait ThreadGroupReferenceCommand extends Command
case object ThreadGroupReference extends CommandSet {
	case class Name(data: ByteVector) extends ThreadGroupReferenceCommand
	case class Parent(data: ByteVector) extends ThreadGroupReferenceCommand
	case class Children(data: ByteVector) extends ThreadGroupReferenceCommand
}

sealed trait ArrayReferenceCommand extends Command
case object ArrayReference extends CommandSet {
	case class Length(data: ByteVector) extends ArrayReferenceCommand
	case class GetValues(data: ByteVector) extends ArrayReferenceCommand
	case class SetValues(data: ByteVector) extends ArrayReferenceCommand
}

sealed trait ClassLoaderReferenceCommand extends Command
case object ClassLoaderReference extends CommandSet {
	case class VisibleClasses(data: ByteVector) extends ClassLoaderReferenceCommand
}

sealed trait EventRequestCommand extends Command
case object EventRequest extends CommandSet {
	case class Set(data: ByteVector) extends EventRequestCommand
	case class Clear(eventKind: Byte, requestId: Int) extends EventRequestCommand
	case class ClearAllBreakpoints(data: ByteVector) extends EventRequestCommand
}

sealed trait StackFrameCommand extends Command
case object StackFrame extends CommandSet {
	case class GetValues(data: ByteVector) extends StackFrameCommand
	case class SetValues(data: ByteVector) extends StackFrameCommand
	case class ThisObject(data: ByteVector) extends StackFrameCommand
	case class PopFrames(data: ByteVector) extends StackFrameCommand
}

sealed trait ClassObjectReferenceCommand extends Command
case object ClassObjectReference extends CommandSet {
	case class ReflectedType(data: ByteVector) extends ClassObjectReferenceCommand
}

sealed trait EventCommand extends Command
case object Event extends CommandSet {
	case class Composite(suspendPolicy: Byte, events: Seq[CompositeEvent]) extends EventCommand
}

object CommandCodecs {	
	def command(implicit idSizes: IdSizes): Codec[Command] = codecs.discriminated[Command].by(codecs.uint8)
		.\ (1) {case in: VirtualMachineCommand => in} { codecs.discriminated[VirtualMachineCommand].by(codecs.uint8)
			.\ (1) {case in: VirtualMachine.Version => in} (codecs.bytes.hlist.as[VirtualMachine.Version])
			.\ (2) {case in: VirtualMachine.ClassesBySignature => in} (codecs.bytes.hlist.as[VirtualMachine.ClassesBySignature])
			.\ (3) {case in: VirtualMachine.AllClasses => in} (codecs.bytes.hlist.as[VirtualMachine.AllClasses])
			.\ (4) {case in: VirtualMachine.AllThreads => in} (codecs.bytes.hlist.as[VirtualMachine.AllThreads])
			.\ (5) {case in: VirtualMachine.TopLevelThreadGroups => in} (codecs.bytes.hlist.as[VirtualMachine.TopLevelThreadGroups])
			.\ (6) {case in: VirtualMachine.Dispose => in} (codecs.bytes.hlist.as[VirtualMachine.Dispose])
			.\ (7) {case in: VirtualMachine.IDSizes => in} (codecs.bytes.hlist.as[VirtualMachine.IDSizes])
			.\ (8) {case in: VirtualMachine.Suspend => in} (codecs.bytes.hlist.as[VirtualMachine.Suspend])
			.\ (9) {case in: VirtualMachine.Resume => in} (codecs.bytes.hlist.as[VirtualMachine.Resume])
			.\ (10) {case in: VirtualMachine.Exit => in} (codecs.bytes.hlist.as[VirtualMachine.Exit])
			.\ (11) {case in: VirtualMachine.CreateString => in} (codecs.bytes.hlist.as[VirtualMachine.CreateString])
			.\ (12) {case in: VirtualMachine.Capabilities => in} (codecs.bytes.hlist.as[VirtualMachine.Capabilities])
			.\ (13) {case in: VirtualMachine.ClassPaths => in} (codecs.bytes.hlist.as[VirtualMachine.ClassPaths])
			.\ (14) {case in: VirtualMachine.DisposeObjects => in} (codecs.bytes.hlist.as[VirtualMachine.DisposeObjects])
			.\ (15) {case in: VirtualMachine.HoldEvents => in} (codecs.bytes.hlist.as[VirtualMachine.HoldEvents])
			.\ (16) {case in: VirtualMachine.ReleaseEvents => in} (codecs.bytes.hlist.as[VirtualMachine.ReleaseEvents])
			.\ (17) {case in: VirtualMachine.CapabilitiesNew => in} (codecs.bytes.hlist.as[VirtualMachine.CapabilitiesNew])
			.\ (18) {case in: VirtualMachine.RedefineClasses => in} (codecs.bytes.hlist.as[VirtualMachine.RedefineClasses])
			.\ (19) {case in: VirtualMachine.SetDefaultStratum => in} (codecs.bytes.hlist.as[VirtualMachine.SetDefaultStratum])
			.\ (20) {case in: VirtualMachine.AllClassesWithGeneric => in} (codecs.bytes.hlist.as[VirtualMachine.AllClassesWithGeneric])
			.\ (21) {case in: VirtualMachine.InstanceCounts => in} (codecs.bytes.hlist.as[VirtualMachine.InstanceCounts])
		}
		.\ (2) {case in: ReferenceTypeCommand => in} { codecs.discriminated[ReferenceTypeCommand].by(codecs.uint8)
			.\ (1) {case in: ReferenceType.Signature => in} (codecs.bytes.hlist.as[ReferenceType.Signature])
			.\ (2) {case in: ReferenceType.ClassLoader => in} (codecs.bytes.hlist.as[ReferenceType.ClassLoader])
			.\ (3) {case in: ReferenceType.Modifiers => in} (codecs.bytes.hlist.as[ReferenceType.Modifiers])
			.\ (4) {case in: ReferenceType.Fields => in} (codecs.bytes.hlist.as[ReferenceType.Fields])
			.\ (5) {case in: ReferenceType.Methods => in} (codecs.bytes.hlist.as[ReferenceType.Methods])
			.\ (6) {case in: ReferenceType.GetValues => in} (codecs.bytes.hlist.as[ReferenceType.GetValues])
			.\ (7) {case in: ReferenceType.SourceFile => in} (codecs.bytes.hlist.as[ReferenceType.SourceFile])
			.\ (8) {case in: ReferenceType.NestedTypes => in} (codecs.bytes.hlist.as[ReferenceType.NestedTypes])
			.\ (9) {case in: ReferenceType.Status => in} (codecs.bytes.hlist.as[ReferenceType.Status])
			.\ (10) {case in: ReferenceType.Interfaces => in} (codecs.bytes.hlist.as[ReferenceType.Interfaces])
			.\ (11) {case in: ReferenceType.ClassObject => in} (codecs.bytes.hlist.as[ReferenceType.ClassObject])
			.\ (12) {case in: ReferenceType.SourceDebugExtension => in} (codecs.bytes.hlist.as[ReferenceType.SourceDebugExtension])
			.\ (13) {case in: ReferenceType.SignatureWithGeneric => in} (codecs.bytes.hlist.as[ReferenceType.SignatureWithGeneric])
			.\ (14) {case in: ReferenceType.FieldsWithGeneric => in} (codecs.bytes.hlist.as[ReferenceType.FieldsWithGeneric])
			.\ (15) {case in: ReferenceType.MethodsWithGeneric => in} (codecs.bytes.hlist.as[ReferenceType.MethodsWithGeneric])
			.\ (16) {case in: ReferenceType.Instances => in} (codecs.bytes.hlist.as[ReferenceType.Instances])
			.\ (17) {case in: ReferenceType.ClassFileVersion => in} (codecs.bytes.hlist.as[ReferenceType.ClassFileVersion])
			.\ (18) {case in: ReferenceType.ConstantPool => in} (codecs.bytes.hlist.as[ReferenceType.ConstantPool])
		}
		.\ (3) {case in: ClassTypeCommand => in} { codecs.discriminated[ClassTypeCommand].by(codecs.uint8)
			.\ (1) {case in: ClassType.Superclass => in} (codecs.bytes.hlist.as[ClassType.Superclass])
			.\ (2) {case in: ClassType.SetValues => in} (codecs.bytes.hlist.as[ClassType.SetValues])
			.\ (3) {case in: ClassType.InvokeMethod => in} (codecs.bytes.hlist.as[ClassType.InvokeMethod])
			.\ (4) {case in: ClassType.NewInstance => in} (codecs.bytes.hlist.as[ClassType.NewInstance])
		}
		.\ (4) {case in: ArrayTypeCommand => in} { codecs.discriminated[ArrayTypeCommand].by(codecs.uint8)
			.\ (1) {case in: ArrayType.NewInstance => in} (codecs.bytes.hlist.as[ArrayType.NewInstance])
		}
		.\ (5) {case in: InterfaceTypeCommand => in} { codecs.discriminated[InterfaceTypeCommand].by(codecs.uint8) }
		.\ (6) {case in: MethodCommand => in} { codecs.discriminated[MethodCommand].by(codecs.uint8)
			.\ (1) {case in: Method.LineTable => in} (codecs.bytes.hlist.as[Method.LineTable])
			.\ (2) {case in: Method.VariableTable => in} (codecs.bytes.hlist.as[Method.VariableTable])
			.\ (3) {case in: Method.Bytecodes => in} (codecs.bytes.hlist.as[Method.Bytecodes])
			.\ (4) {case in: Method.IsObsolete => in} (codecs.bytes.hlist.as[Method.IsObsolete])
			.\ (5) {case in: Method.VariableTableWithGeneric => in} (codecs.bytes.hlist.as[Method.VariableTableWithGeneric])
		}
		.\ (8) {case in: FieldCommand => in} { codecs.discriminated[FieldCommand].by(codecs.uint8) }
		.\ (9) {case in: ObjectReferenceCommand => in} { codecs.discriminated[ObjectReferenceCommand].by(codecs.uint8)
			.\ (1) {case in: ObjectReference.ReferenceType => in} (codecs.bytes.hlist.as[ObjectReference.ReferenceType])
			.\ (2) {case in: ObjectReference.GetValues => in} (codecs.bytes.hlist.as[ObjectReference.GetValues])
			.\ (3) {case in: ObjectReference.SetValues => in} (codecs.bytes.hlist.as[ObjectReference.SetValues])
			.\ (5) {case in: ObjectReference.MonitorInfo => in} (codecs.bytes.hlist.as[ObjectReference.MonitorInfo])
			.\ (6) {case in: ObjectReference.InvokeMethod => in} (codecs.bytes.hlist.as[ObjectReference.InvokeMethod])
			.\ (7) {case in: ObjectReference.DisableCollection => in} (codecs.bytes.hlist.as[ObjectReference.DisableCollection])
			.\ (8) {case in: ObjectReference.EnableCollection => in} (codecs.bytes.hlist.as[ObjectReference.EnableCollection])
			.\ (9) {case in: ObjectReference.IsCollected => in} (codecs.bytes.hlist.as[ObjectReference.IsCollected])
			.\ (10) {case in: ObjectReference.ReferringObjects => in} (codecs.bytes.hlist.as[ObjectReference.ReferringObjects])
		}
		.\ (10) {case in: StringReferenceCommand => in} { codecs.discriminated[StringReferenceCommand].by(codecs.uint8)
			.\ (1) {case in: StringReference.Value => in} (codecs.bytes.hlist.as[StringReference.Value])
		}
		.\ (11) {case in: ThreadReferenceCommand => in} { codecs.discriminated[ThreadReferenceCommand].by(codecs.uint8)
			.\ (1) {case in: ThreadReference.Name => in} (codecs.bytes.hlist.as[ThreadReference.Name])
			.\ (2) {case in: ThreadReference.Suspend => in} (codecs.bytes.hlist.as[ThreadReference.Suspend])
			.\ (3) {case in: ThreadReference.Resume => in} (codecs.bytes.hlist.as[ThreadReference.Resume])
			.\ (4) {case in: ThreadReference.Status => in} (codecs.bytes.hlist.as[ThreadReference.Status])
			.\ (5) {case in: ThreadReference.ThreadGroup => in} (codecs.bytes.hlist.as[ThreadReference.ThreadGroup])
			.\ (6) {case in: ThreadReference.Frames => in} (codecs.bytes.hlist.as[ThreadReference.Frames])
			.\ (7) {case in: ThreadReference.FrameCount => in} (codecs.bytes.hlist.as[ThreadReference.FrameCount])
			.\ (8) {case in: ThreadReference.OwnedMonitors => in} (codecs.bytes.hlist.as[ThreadReference.OwnedMonitors])
			.\ (9) {case in: ThreadReference.CurrentContendedMonitor => in} (codecs.bytes.hlist.as[ThreadReference.CurrentContendedMonitor])
			.\ (10) {case in: ThreadReference.Stop => in} (codecs.bytes.hlist.as[ThreadReference.Stop])
			.\ (11) {case in: ThreadReference.Interrupt => in} (codecs.bytes.hlist.as[ThreadReference.Interrupt])
			.\ (12) {case in: ThreadReference.SuspendCount => in} (codecs.bytes.hlist.as[ThreadReference.SuspendCount])
			.\ (13) {case in: ThreadReference.OwnedMonitorsStackDepthInfo => in} (codecs.bytes.hlist.as[ThreadReference.OwnedMonitorsStackDepthInfo])
			.\ (14) {case in: ThreadReference.ForceEarlyReturn => in} (codecs.bytes.hlist.as[ThreadReference.ForceEarlyReturn])
		}
		.\ (12) {case in: ThreadGroupReferenceCommand => in} { codecs.discriminated[ThreadGroupReferenceCommand].by(codecs.uint8)
			.\ (1) {case in: ThreadGroupReference.Name => in} (codecs.bytes.hlist.as[ThreadGroupReference.Name])
			.\ (2) {case in: ThreadGroupReference.Parent => in} (codecs.bytes.hlist.as[ThreadGroupReference.Parent])
			.\ (3) {case in: ThreadGroupReference.Children => in} (codecs.bytes.hlist.as[ThreadGroupReference.Children])
		}
		.\ (13) {case in: ArrayReferenceCommand => in} { codecs.discriminated[ArrayReferenceCommand].by(codecs.uint8)
			.\ (1) {case in: ArrayReference.Length => in} (codecs.bytes.hlist.as[ArrayReference.Length])
			.\ (2) {case in: ArrayReference.GetValues => in} (codecs.bytes.hlist.as[ArrayReference.GetValues])
			.\ (3) {case in: ArrayReference.SetValues => in} (codecs.bytes.hlist.as[ArrayReference.SetValues])
		}
		.\ (14) {case in: ClassLoaderReferenceCommand => in} { codecs.discriminated[ClassLoaderReferenceCommand].by(codecs.uint8)
			.\ (1) {case in: ClassLoaderReference.VisibleClasses => in} (codecs.bytes.hlist.as[ClassLoaderReference.VisibleClasses])
		}
		.\ (15) {case in: EventRequestCommand => in} { codecs.discriminated[EventRequestCommand].by(codecs.uint8)
			.\ (1) {case in: EventRequest.Set => in} (codecs.bytes.hlist.as[EventRequest.Set])
			.\ (2) {case in: EventRequest.Clear => in} ((byte :: int).as[EventRequest.Clear])
			.\ (3) {case in: EventRequest.ClearAllBreakpoints => in} (codecs.bytes.hlist.as[EventRequest.ClearAllBreakpoints])
		}
		.\ (16) {case in: StackFrameCommand => in} { codecs.discriminated[StackFrameCommand].by(codecs.uint8)
			.\ (1) {case in: StackFrame.GetValues => in} (codecs.bytes.hlist.as[StackFrame.GetValues])
			.\ (2) {case in: StackFrame.SetValues => in} (codecs.bytes.hlist.as[StackFrame.SetValues])
			.\ (3) {case in: StackFrame.ThisObject => in} (codecs.bytes.hlist.as[StackFrame.ThisObject])
			.\ (4) {case in: StackFrame.PopFrames => in} (codecs.bytes.hlist.as[StackFrame.PopFrames])
		}
		.\ (17) {case in: ClassObjectReferenceCommand => in} { codecs.discriminated[ClassObjectReferenceCommand].by(codecs.uint8)
			.\ (1) {case in: ClassObjectReference.ReflectedType => in} (codecs.bytes.hlist.as[ClassObjectReference.ReflectedType])
		}
		.\ (64) {case in: EventCommand => in} { codecs.discriminated[EventCommand].by(codecs.uint8)
			.\ (100) {case in: Event.Composite => in} ((byte :: CommandBody.compositeEvents).as[Event.Composite])
		}
}
