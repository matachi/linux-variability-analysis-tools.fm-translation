name := "fm-translation"

version := "0.5-SNAPSHOT"

organization := "ca.uwaterloo.gsd"

scalaVersion := "2.9.0-1"

libraryDependencies ++= Seq(
    "ca.uwaterloo.gsd" % "lvat" % "0.5-SNAPSHOT",
    "org.clapper" %% "argot" % "0.3.3",
    "org.sat4j" % "org.sat4j.core" % "2.1.1",
    "com.novocode" % "junit-interface" % "0.6" % "test",
    "junit" % "junit" % "4.8.2" % "test",
    "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test",
    "org.scala-tools.testing" % "scalacheck_2.9.0-1" % "1.9" % "test"
)

resolvers += "Local Maven Repository" at Path.userHome.asURL + "/.m2/repository"

// only show 10 lines of stack traces
traceLevel := 10

javaOptions += "-Xss8192k -Xmx2048m"

