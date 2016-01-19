name := "fm-translation"

version := "0.5-SNAPSHOT"

organization := "ca.uwaterloo.gsd"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
    "org.clapper" %% "argot" % "1.0.4",
    "org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5",
    //"org.sat4j" % "org.sat4j.core" % "2.3.1",
    "com.novocode" % "junit-interface" % "0.11" % "test",
    "junit" % "junit" % "4.12",
    "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
    //"org.scala-tools.testing" % "scalacheck_2.9.0-1" % "1.9" % "test"
    "org.scalacheck" % "scalacheck_2.11" % "1.11.6",
    "com.typesafe.scala-logging" % "scala-logging_2.11" % "3.1.0",
    "com.googlecode.kiama" % "kiama_2.11" % "1.8.0"
)

resolvers += "Local Maven Repository" at Path.userHome.asURL + "/.m2/repository"

// only show 10 lines of stack traces
traceLevel in run := 10

// fork a new JVM for 'run' and 'test:run'
fork in run := true

// options to use when forking
javaOptions ++= Seq("-Xmx2048m", "-Xss4096k")

