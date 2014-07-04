import sbt._
import sbt.Keys._
import java.net.Socket
import java.net.ServerSocket
import java.nio.ByteBuffer
import scala.util.Try
import java.io.InputStream
import java.io.BufferedInputStream

object DebugPlugin extends Plugin {
    val debugStart = taskKey[Unit]("starts debugger proxy")
    val debugStop = taskKey[Unit]("stops debugger proxy")
    val debugPort = settingKey[Int]("port to attache your debugger to")
    val debugClientPorts = settingKey[Set[Int]]("open debugger ports for child applications")

    private val debugProxyAttribute = AttributeKey[DebugProxy]("debugProxy")

    val debugSettings = Seq(
        debugPort := 6006,
        debugClientPorts := Set(6007),
        onLoad in Global := {
            (onLoad in Global).value.andThen { state =>
                val proxy = new DebugProxy(debugPort.value, debugClientPorts.value)
                state.put(debugProxyAttribute, proxy)
            }
        },
        onUnload in Global := {
            (onUnload in Global).value.andThen { state =>
                state.get(debugProxyAttribute).foreach { proxy =>
                    proxy.stop
                }
                state.remove(debugProxyAttribute)
            }
        },
        debugStart := {
            state.value.get(debugProxyAttribute).foreach { proxy =>
                proxy.start
            }
        },
        debugStop := {
            state.value.get(debugProxyAttribute).foreach { proxy =>
                proxy.stop
            }
        }
    )

    class DebugProxy(port: Int, val clientPorts: Set[Int]) {
        val HANDSHAKE = "JDWP-Handshake".toCharArray.map(_.toByte)
        private var server: ServerSocket = null
        private var thread: DebugThread = null
        private var connectedClients = Map[Int, Socket]()
        private var runThread = true

        private var replayMessages = Vector[Array[Byte]]()

        def stop(): Unit = {
            runThread = false
            server.close
            connectedClients.values.foreach(_.close)
            connectedClients = Map[Int, Socket]()
            replayMessages = Vector[Array[Byte]]()
        }

        def start(): Unit = {
            runThread = true
            server = new ServerSocket(port)
            thread = new DebugThread()

            thread.start
        }

        private def getNewClientSockets(): Map[Int, Socket] = {
            val unConnectedClientPorts = clientPorts -- connectedClients.keys
            val connectionAttempts = unConnectedClientPorts.map { port =>
                val attempt = Try(new Socket(null: String, port))

                (port, attempt)
            }.toMap

            val successfulConnections = connectionAttempts.filter { case (port, attempt) =>
                attempt.isSuccess
            }

            val newSocketMap = successfulConnections.mapValues(_.get)

            newSocketMap.values.foreach { socket =>
                socket.getOutputStream.write(HANDSHAKE)
                socket.getInputStream.skip(HANDSHAKE.length)

                replayMessages.foreach { message =>
                    socket.getOutputStream.write(message)
                }
            }

            newSocketMap
        }

        private def readMessage(inputStream: InputStream): Array[Byte] = {
            if(inputStream.available > 4) {
                val lengthBytes = Array.ofDim[Byte](4)
                inputStream.read(lengthBytes)

                val messageLength = ByteBuffer.wrap(lengthBytes).getInt

                val bodyBytes = Array.ofDim[Byte](messageLength - 4)
                inputStream.read(bodyBytes)

                lengthBytes ++ bodyBytes
            }
            else {
                Array.empty[Byte]
            }
        }

        def handleBreakPointMessage(message: Array[Byte]): Unit = {
            if(message.size > 0) {
                // in breakpoint command group
                if(message(9) == 15) {
                    val command = message(10)
                    command match {
                        // set
                        case 1 => replayMessages = replayMessages :+ message
                        // clear
                        case 2 => replayMessages = replayMessages :+ message
                        // clear all
                        case 3 => replayMessages = Vector[Array[Byte]]()
                        case _ => println("invalid breakpoint command")
                    }
                }
            }
        }

        private def peak(inStream: InputStream): Byte = {
            inStream.mark(1)
            val firstByte = inStream.read.asInstanceOf[Byte]
            inStream.reset

            firstByte
        }

        private class DebugThread extends Thread {
            override def run(): Unit = {
                while(runThread) {
                    val debuggerSocket = server.accept

                    val debuggerInput = debuggerSocket.getInputStream
                    val debuggerOutput = debuggerSocket.getOutputStream
                    while(debuggerInput.available < HANDSHAKE.length) {
                        Thread.sleep(100)
                    }
                    debuggerInput.skip(HANDSHAKE.length)
                    debuggerOutput.write(HANDSHAKE)
                    
                    val debuggerReadThread = new DebuggerReadThread(debuggerSocket)
                    debuggerReadThread.start

                    while(!debuggerSocket.isClosed) {
                        // invalidate closed clients
                        connectedClients = connectedClients.filter { case (port, socket) =>
                            !socket.isClosed
                        }

                        // look for new clients
                        val newConnectedClients = getNewClientSockets
                        connectedClients = connectedClients ++ newConnectedClients
                        newConnectedClients.values.foreach { vmSocket =>
                            val vmReadThread = new VmReadThread(debuggerSocket, vmSocket)
                            vmReadThread.start
                        }

                        Thread.sleep(100)
                    }

                    debuggerSocket.close

                    replayMessages = Vector[Array[Byte]]()

                    connectedClients.foreach { case (port, socket) =>
                        socket.close
                    }
                    connectedClients = Map[Int, Socket]()
                }
            }
        }

        private class DebuggerReadThread(debuggerSocket: Socket) extends Thread {
            override def run: Unit = {
                val inputStream = new BufferedInputStream(debuggerSocket.getInputStream)

                while(!debuggerSocket.isClosed) {
                    val firstByte = peak(inputStream)
                    if(firstByte == -1) {
                        debuggerSocket.close
                    } else {
                        // send messages from debugger to VMs
                        if(connectedClients.size > 0) {
                            val debuggerMessage = readMessage(inputStream)
if(debuggerMessage.length > 0) println("DEBUGGER MESSAGE: " + debuggerMessage.mkString(" "))
                            connectedClients.values.foreach { vmSocket =>
                                val vmOutputStream = vmSocket.getOutputStream
                                vmOutputStream.write(debuggerMessage)
                            }

                            // save breakpoint messages
                            handleBreakPointMessage(debuggerMessage)
                        }
                    }
                }
            }
        }

        private class VmReadThread(debuggerSocket: Socket, vmSocket: Socket) extends Thread {
            override def run: Unit = {
                val inputStream = new BufferedInputStream(vmSocket.getInputStream)
                while(!vmSocket.isClosed) {
                    val firstByte = peak(inputStream)
                    if(firstByte == -1) {
                        vmSocket.close
                    } else {
                        // send messages from VMs to debugger
                        val debuggerOutputStream = debuggerSocket.getOutputStream
                        val vmMessage = readMessage(inputStream)
if(vmMessage.length > 0) println("VM MESSAGE: " + vmMessage.mkString(" "))
                        debuggerOutputStream.write(vmMessage)
                    }
                }
            }
        }
    }
}
