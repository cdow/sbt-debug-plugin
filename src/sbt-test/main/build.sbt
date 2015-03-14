fork := true

javaOptions in run ++= Seq("-Xmx256m", "-agentlib:jdwp=transport=dt_socket,address=6007,server=n,suspend=n")

scalacOptions += "-g:vars"

debugSettings

logLevel in debugStart := Level.Debug
