package gsd.linux.tools

trait Project {
  val inFile: String
  val outFile: String
}

trait LinuxProject extends Project {
  val inFile = "input/2.6.28.6-edited.exconfig"
  val outFile = "input/2.6.28.6.dimacs"
}
trait BusyboxProject extends Project {
  val inFile = "input/busybox-1.17.2.exconfig"
  val outFile = "input/busybox-1.17.2.dimacs"
}
