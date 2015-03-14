package com.github.cdow

import com.typesafe.config.ConfigFactory
import sbt._
import sbt.Keys._
import akka.actor.ActorSystem
import com.github.cdow.actor.MainActor

object DebugPlugin extends Plugin {
	val debugStart = taskKey[Unit]("starts debugger proxy")
	val debugStop = taskKey[Unit]("stops debugger proxy")
	val debugPort = settingKey[Int]("port to attache your debugger to")
	val debugVmPort = settingKey[Int]("VM debug port to connect to")

	private val debugProxyAttribute = AttributeKey[DebugProxy]("debugProxy")

	val debugSettings = Seq(
		debugPort := 6006,
		debugVmPort := 6007,
		onLoad in Global := {
			(onLoad in Global).value.andThen { state =>
				val proxy = new DebugProxy(debugPort.value, debugVmPort.value)
				state.put(debugProxyAttribute, proxy)
			}
		},
		onUnload in Global := {
			(onUnload in Global).value.andThen { state =>
				state.get(debugProxyAttribute).foreach { proxy =>
					proxy.stop()
				}
				state.remove(debugProxyAttribute)
			}
		},
		debugStart := {
			val logger = streams.value.log
			state.value.get(debugProxyAttribute).foreach { proxy =>
				proxy.start(logger)
			}
		},
		debugStop := {
			state.value.get(debugProxyAttribute).foreach { proxy =>
				proxy.stop()
			}
		})

	class DebugProxy(debuggerPort: Int, val vmPort: Int) {
		var actorSystem: Option[ActorSystem] = None

		def stop(): Unit = {
			actorSystem.foreach(_.shutdown())
			actorSystem = None
		}

		def start(logger: Logger): Unit = {
			actorSystem = actorSystem.orElse {
				val config = ConfigFactory.parseString("")
				// Default config does not load unless we specify a custom classloader
				val system = ActorSystem("main", config, getClass.getClassLoader)
				system.actorOf(MainActor.props(debuggerPort, vmPort, logger))
				Some(system)
			}
		}
	}
}
