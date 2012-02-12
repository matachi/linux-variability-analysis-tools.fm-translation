name := "fm-translation"

version := "0.5-SNAPSHOT"

organization := "ca.uwaterloo.gsd"

scalaVersion := "2.9.0-1"

libraryDependencies ++= Seq(
    "org.clapper" %% "argot" % "0.3.5",
    "org.sat4j" % "org.sat4j.core" % "2.3.0",
    "com.novocode" % "junit-interface" % "0.6" % "test",
    "junit" % "junit" % "4.8.2" % "test",
    "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test",
    "org.scala-tools.testing" % "scalacheck_2.9.0-1" % "1.9" % "test"
)

resolvers += "Local Maven Repository" at Path.userHome.asURL + "/.m2/repository"

// only show 10 lines of stack traces
traceLevel in run := 10

// fork a new JVM for 'run' and 'test:run'
fork in run := true

// options to use when forking
javaOptions += "-Xmx2048m"

javaOptions += "-Xss4096k"

