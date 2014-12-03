package com.github.cdow
import scodec.bits.ByteVector
import com.github.cdow.commands.Command

case class JdwpPacket(id: Long, message: JdwpMessage)

sealed trait JdwpMessage
case class CommandPacket(command: Command) extends JdwpMessage
case class ResponsePacket(errorCode: Int,data: ByteVector) extends JdwpMessage