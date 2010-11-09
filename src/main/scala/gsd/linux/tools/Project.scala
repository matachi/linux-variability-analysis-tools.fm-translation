package gsd.linux.tools

// TODO maybe move this to a package object
object Projects {
  val projects = Map(
    "linux" -> LinuxProject,
    "busybox" -> BusyboxProject
  )
}

trait Project {
  val base: String

  lazy val exconfigFile: String = base + ".exconfig"
  lazy val dimacsFile: String = base + ".dimacs"
  lazy val implgFile: String = base + ".implg"
  lazy val graphvizFile: String = base + ".dot"
}

object LinuxProject extends Project {
  val base = "input/2.6.28.6"
  override lazy val exconfigFile = "input/2.6.28.6-edited.exconfig"
}

object BusyboxProject extends Project {
  val base = "input/busybox-1.17.2"
}
