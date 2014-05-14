import sbt._
import sbt.Keys._
import java.net.Socket
import java.net.ServerSocket
import java.nio.ByteBuffer

object DebugPlugin extends Plugin {
    val debugStart = taskKey[Unit]("starts debugger proxy")
    val debugStop = taskKey[Unit]("stops debugger proxy")
    val debugPort = settingKey[Int]("port to attache your debugger to")
    val debugClientPorts = settingKey[Set[Int]]("open debugger ports for child applications")

    private val debugProxyAttribute = AttributeKey[DebugProxy]("debugProxy")

    val debugSettings = Seq(
        debugPort := 6006,
        debugClientPorts := Set(6007),
        debugStart := {
            val proxy = state.value.get(debugProxyAttribute).getOrElse(
                new DebugProxy(debugPort.value, debugClientPorts.value)
            )
            state.value.put(debugProxyAttribute, proxy)
        },
        debugStop := {
            state.value.get(debugProxyAttribute).foreach { proxy =>
                proxy.stop
            }
            state.value.remove(debugProxyAttribute)
        }
    )

    class DebugProxy(port: Int, val clientPorts: Set[Int]) {
        val server = new ServerSocket(port)
        private val thread = new DebugThread()
        private var connectedClients = Map[Int, Socket]()
        private var runThread = true

        thread.start()

        def stop(): Unit = {
            runThread = false
            server.close
            connectedClients.values.foreach(_.close)
        }

        private def getNewClientSockets(): Map[Int, Socket] = {
            val unConnectedClientPorts = clientPorts -- connectedClients.keys
            val connectionAttempts = unConnectedClientPorts.map { port =>
                val attempt = new Socket(null: String, port)

                (port, attempt)
            }.toMap

            connectionAttempts.filter { case (port, attempt) =>
                attempt.isConnected && !attempt.isClosed
            }
            //TODO replay breakpoints
        }

        private def readMessage(socket: Socket): Array[Byte] = {
            val inputStream = socket.getInputStream
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
                        case 1 => // TODO
                        // clear
                        case 2 => // TODO
                        // clear all
                        case 3 => // TODO
                        case _ => println("invalid breakpoint command")
                    }
                }
            }
        }

        private class DebugThread extends Thread {
            override def run(): Unit = {
                while(runThread) {
                    val debuggerSocket = server.accept

                    while(debuggerSocket.isConnected && !debuggerSocket.isClosed) {
                        // invalidate closed clients
                        connectedClients = connectedClients.filter { case (port, socket) =>
                            socket.isConnected && !socket.isClosed
                        }

                        // look for new clients
                        val newConnectedClients = getNewClientSockets
                        connectedClients = connectedClients ++ newConnectedClients

                        // send messages from debugger to clients
                        val debuggerMessage = readMessage(debuggerSocket)
                        connectedClients.values.foreach { clientSocket =>
                            val clientOutputStream = clientSocket.getOutputStream
                            clientOutputStream.write(debuggerMessage)
                        }

                        // save breakpoint messages
                        handleBreakPointMessage(debuggerMessage)

                        // send messages from clients to debugger
                        val debuggerOutputStream = debuggerSocket.getOutputStream
                        connectedClients.values.foreach { clientSocket =>
                            val clientMessage = readMessage(clientSocket)
                            debuggerOutputStream.write(clientMessage)
                        }
                    }

                    debuggerSocket.close

                    // TODO clear breakpoints
                    connectedClients.foreach { case (port, socket) =>
                        socket.close
                    }
                }
            }
        }
    }
}
