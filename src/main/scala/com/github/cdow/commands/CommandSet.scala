package com.github.cdow.commands

import scodec.Codec
import scodec.bits.{ByteVector, BitVector}
import scodec.codecs._

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
	case class Clear(data: ByteVector) extends EventRequestCommand
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
	case class Composite(data: ByteVector) extends EventCommand
}

object CommandCodecs {	
	val command: Codec[Command] = discriminated[Command].by(uint8)
		.\ (1) {case in: VirtualMachineCommand => in} { discriminated[VirtualMachineCommand].by(uint8)
			.\ (1) {case in: VirtualMachine.Version => in} (bytes.hlist.as[VirtualMachine.Version])
			.\ (2) {case in: VirtualMachine.ClassesBySignature => in} (bytes.hlist.as[VirtualMachine.ClassesBySignature])
			.\ (3) {case in: VirtualMachine.AllClasses => in} (bytes.hlist.as[VirtualMachine.AllClasses])
			.\ (4) {case in: VirtualMachine.AllThreads => in} (bytes.hlist.as[VirtualMachine.AllThreads])
			.\ (5) {case in: VirtualMachine.TopLevelThreadGroups => in} (bytes.hlist.as[VirtualMachine.TopLevelThreadGroups])
			.\ (6) {case in: VirtualMachine.Dispose => in} (bytes.hlist.as[VirtualMachine.Dispose])
			.\ (7) {case in: VirtualMachine.IDSizes => in} (bytes.hlist.as[VirtualMachine.IDSizes])
			.\ (8) {case in: VirtualMachine.Suspend => in} (bytes.hlist.as[VirtualMachine.Suspend])
			.\ (9) {case in: VirtualMachine.Resume => in} (bytes.hlist.as[VirtualMachine.Resume])
			.\ (10) {case in: VirtualMachine.Exit => in} (bytes.hlist.as[VirtualMachine.Exit])
			.\ (11) {case in: VirtualMachine.CreateString => in} (bytes.hlist.as[VirtualMachine.CreateString])
			.\ (12) {case in: VirtualMachine.Capabilities => in} (bytes.hlist.as[VirtualMachine.Capabilities])
			.\ (13) {case in: VirtualMachine.ClassPaths => in} (bytes.hlist.as[VirtualMachine.ClassPaths])
			.\ (14) {case in: VirtualMachine.DisposeObjects => in} (bytes.hlist.as[VirtualMachine.DisposeObjects])
			.\ (15) {case in: VirtualMachine.HoldEvents => in} (bytes.hlist.as[VirtualMachine.HoldEvents])
			.\ (16) {case in: VirtualMachine.ReleaseEvents => in} (bytes.hlist.as[VirtualMachine.ReleaseEvents])
			.\ (17) {case in: VirtualMachine.CapabilitiesNew => in} (bytes.hlist.as[VirtualMachine.CapabilitiesNew])
			.\ (18) {case in: VirtualMachine.RedefineClasses => in} (bytes.hlist.as[VirtualMachine.RedefineClasses])
			.\ (19) {case in: VirtualMachine.SetDefaultStratum => in} (bytes.hlist.as[VirtualMachine.SetDefaultStratum])
			.\ (20) {case in: VirtualMachine.AllClassesWithGeneric => in} (bytes.hlist.as[VirtualMachine.AllClassesWithGeneric])
			.\ (21) {case in: VirtualMachine.InstanceCounts => in} (bytes.hlist.as[VirtualMachine.InstanceCounts])
		}
		.\ (2) {case in: ReferenceTypeCommand => in} { discriminated[ReferenceTypeCommand].by(uint8)
			.\ (1) {case in: ReferenceType.Signature => in} (bytes.hlist.as[ReferenceType.Signature])
			.\ (2) {case in: ReferenceType.ClassLoader => in} (bytes.hlist.as[ReferenceType.ClassLoader])
			.\ (3) {case in: ReferenceType.Modifiers => in} (bytes.hlist.as[ReferenceType.Modifiers])
			.\ (4) {case in: ReferenceType.Fields => in} (bytes.hlist.as[ReferenceType.Fields])
			.\ (5) {case in: ReferenceType.Methods => in} (bytes.hlist.as[ReferenceType.Methods])
			.\ (6) {case in: ReferenceType.GetValues => in} (bytes.hlist.as[ReferenceType.GetValues])
			.\ (7) {case in: ReferenceType.SourceFile => in} (bytes.hlist.as[ReferenceType.SourceFile])
			.\ (8) {case in: ReferenceType.NestedTypes => in} (bytes.hlist.as[ReferenceType.NestedTypes])
			.\ (9) {case in: ReferenceType.Status => in} (bytes.hlist.as[ReferenceType.Status])
			.\ (10) {case in: ReferenceType.Interfaces => in} (bytes.hlist.as[ReferenceType.Interfaces])
			.\ (11) {case in: ReferenceType.ClassObject => in} (bytes.hlist.as[ReferenceType.ClassObject])
			.\ (12) {case in: ReferenceType.SourceDebugExtension => in} (bytes.hlist.as[ReferenceType.SourceDebugExtension])
			.\ (13) {case in: ReferenceType.SignatureWithGeneric => in} (bytes.hlist.as[ReferenceType.SignatureWithGeneric])
			.\ (14) {case in: ReferenceType.FieldsWithGeneric => in} (bytes.hlist.as[ReferenceType.FieldsWithGeneric])
			.\ (15) {case in: ReferenceType.MethodsWithGeneric => in} (bytes.hlist.as[ReferenceType.MethodsWithGeneric])
			.\ (16) {case in: ReferenceType.Instances => in} (bytes.hlist.as[ReferenceType.Instances])
			.\ (17) {case in: ReferenceType.ClassFileVersion => in} (bytes.hlist.as[ReferenceType.ClassFileVersion])
			.\ (18) {case in: ReferenceType.ConstantPool => in} (bytes.hlist.as[ReferenceType.ConstantPool])
		}
		.\ (3) {case in: ClassTypeCommand => in} { discriminated[ClassTypeCommand].by(uint8)
			.\ (1) {case in: ClassType.Superclass => in} (bytes.hlist.as[ClassType.Superclass])
			.\ (2) {case in: ClassType.SetValues => in} (bytes.hlist.as[ClassType.SetValues])
			.\ (3) {case in: ClassType.InvokeMethod => in} (bytes.hlist.as[ClassType.InvokeMethod])
			.\ (4) {case in: ClassType.NewInstance => in} (bytes.hlist.as[ClassType.NewInstance])
		}
		.\ (4) {case in: ArrayTypeCommand => in} { discriminated[ArrayTypeCommand].by(uint8)
			.\ (1) {case in: ArrayType.NewInstance => in} (bytes.hlist.as[ArrayType.NewInstance])
		}
		.\ (5) {case in: InterfaceTypeCommand => in} { discriminated[InterfaceTypeCommand].by(uint8) }
		.\ (6) {case in: MethodCommand => in} { discriminated[MethodCommand].by(uint8)
			.\ (1) {case in: Method.LineTable => in} (bytes.hlist.as[Method.LineTable])
			.\ (2) {case in: Method.VariableTable => in} (bytes.hlist.as[Method.VariableTable])
			.\ (3) {case in: Method.Bytecodes => in} (bytes.hlist.as[Method.Bytecodes])
			.\ (4) {case in: Method.IsObsolete => in} (bytes.hlist.as[Method.IsObsolete])
			.\ (5) {case in: Method.VariableTableWithGeneric => in} (bytes.hlist.as[Method.VariableTableWithGeneric])
		}
		.\ (8) {case in: FieldCommand => in} { discriminated[FieldCommand].by(uint8) }
		.\ (9) {case in: ObjectReferenceCommand => in} { discriminated[ObjectReferenceCommand].by(uint8)
			.\ (1) {case in: ObjectReference.ReferenceType => in} (bytes.hlist.as[ObjectReference.ReferenceType])
			.\ (2) {case in: ObjectReference.GetValues => in} (bytes.hlist.as[ObjectReference.GetValues])
			.\ (3) {case in: ObjectReference.SetValues => in} (bytes.hlist.as[ObjectReference.SetValues])
			.\ (5) {case in: ObjectReference.MonitorInfo => in} (bytes.hlist.as[ObjectReference.MonitorInfo])
			.\ (6) {case in: ObjectReference.InvokeMethod => in} (bytes.hlist.as[ObjectReference.InvokeMethod])
			.\ (7) {case in: ObjectReference.DisableCollection => in} (bytes.hlist.as[ObjectReference.DisableCollection])
			.\ (8) {case in: ObjectReference.EnableCollection => in} (bytes.hlist.as[ObjectReference.EnableCollection])
			.\ (9) {case in: ObjectReference.IsCollected => in} (bytes.hlist.as[ObjectReference.IsCollected])
			.\ (10) {case in: ObjectReference.ReferringObjects => in} (bytes.hlist.as[ObjectReference.ReferringObjects])
		}
		.\ (10) {case in: StringReferenceCommand => in} { discriminated[StringReferenceCommand].by(uint8)
			.\ (1) {case in: StringReference.Value => in} (bytes.hlist.as[StringReference.Value])
		}
		.\ (11) {case in: ThreadReferenceCommand => in} { discriminated[ThreadReferenceCommand].by(uint8)
			.\ (1) {case in: ThreadReference.Name => in} (bytes.hlist.as[ThreadReference.Name])
			.\ (2) {case in: ThreadReference.Suspend => in} (bytes.hlist.as[ThreadReference.Suspend])
			.\ (3) {case in: ThreadReference.Resume => in} (bytes.hlist.as[ThreadReference.Resume])
			.\ (4) {case in: ThreadReference.Status => in} (bytes.hlist.as[ThreadReference.Status])
			.\ (5) {case in: ThreadReference.ThreadGroup => in} (bytes.hlist.as[ThreadReference.ThreadGroup])
			.\ (6) {case in: ThreadReference.Frames => in} (bytes.hlist.as[ThreadReference.Frames])
			.\ (7) {case in: ThreadReference.FrameCount => in} (bytes.hlist.as[ThreadReference.FrameCount])
			.\ (8) {case in: ThreadReference.OwnedMonitors => in} (bytes.hlist.as[ThreadReference.OwnedMonitors])
			.\ (9) {case in: ThreadReference.CurrentContendedMonitor => in} (bytes.hlist.as[ThreadReference.CurrentContendedMonitor])
			.\ (10) {case in: ThreadReference.Stop => in} (bytes.hlist.as[ThreadReference.Stop])
			.\ (11) {case in: ThreadReference.Interrupt => in} (bytes.hlist.as[ThreadReference.Interrupt])
			.\ (12) {case in: ThreadReference.SuspendCount => in} (bytes.hlist.as[ThreadReference.SuspendCount])
			.\ (13) {case in: ThreadReference.OwnedMonitorsStackDepthInfo => in} (bytes.hlist.as[ThreadReference.OwnedMonitorsStackDepthInfo])
			.\ (14) {case in: ThreadReference.ForceEarlyReturn => in} (bytes.hlist.as[ThreadReference.ForceEarlyReturn])
		}
		.\ (12) {case in: ThreadGroupReferenceCommand => in} { discriminated[ThreadGroupReferenceCommand].by(uint8)
			.\ (1) {case in: ThreadGroupReference.Name => in} (bytes.hlist.as[ThreadGroupReference.Name])
			.\ (2) {case in: ThreadGroupReference.Parent => in} (bytes.hlist.as[ThreadGroupReference.Parent])
			.\ (3) {case in: ThreadGroupReference.Children => in} (bytes.hlist.as[ThreadGroupReference.Children])
		}
		.\ (13) {case in: ArrayReferenceCommand => in} { discriminated[ArrayReferenceCommand].by(uint8)
			.\ (1) {case in: ArrayReference.Length => in} (bytes.hlist.as[ArrayReference.Length])
			.\ (2) {case in: ArrayReference.GetValues => in} (bytes.hlist.as[ArrayReference.GetValues])
			.\ (3) {case in: ArrayReference.SetValues => in} (bytes.hlist.as[ArrayReference.SetValues])
		}
		.\ (14) {case in: ClassLoaderReferenceCommand => in} { discriminated[ClassLoaderReferenceCommand].by(uint8)
			.\ (1) {case in: ClassLoaderReference.VisibleClasses => in} (bytes.hlist.as[ClassLoaderReference.VisibleClasses])
		}
		.\ (15) {case in: EventRequestCommand => in} { discriminated[EventRequestCommand].by(uint8)
			.\ (1) {case in: EventRequest.Set => in} (bytes.hlist.as[EventRequest.Set])
			.\ (2) {case in: EventRequest.Clear => in} (bytes.hlist.as[EventRequest.Clear])
			.\ (3) {case in: EventRequest.ClearAllBreakpoints => in} (bytes.hlist.as[EventRequest.ClearAllBreakpoints])
		}
		.\ (16) {case in: StackFrameCommand => in} { discriminated[StackFrameCommand].by(uint8)
			.\ (1) {case in: StackFrame.GetValues => in} (bytes.hlist.as[StackFrame.GetValues])
			.\ (2) {case in: StackFrame.SetValues => in} (bytes.hlist.as[StackFrame.SetValues])
			.\ (3) {case in: StackFrame.ThisObject => in} (bytes.hlist.as[StackFrame.ThisObject])
			.\ (4) {case in: StackFrame.PopFrames => in} (bytes.hlist.as[StackFrame.PopFrames])
		}
		.\ (17) {case in: ClassObjectReferenceCommand => in} { discriminated[ClassObjectReferenceCommand].by(uint8)
			.\ (1) {case in: ClassObjectReference.ReflectedType => in} (bytes.hlist.as[ClassObjectReference.ReflectedType])
		}
		.\ (64) {case in: EventCommand => in} { discriminated[EventCommand].by(uint8)
			.\ (100) {case in: Event.Composite => in} (bytes.hlist.as[Event.Composite])
		}
}
