package gsd.linux.tools

import gsd.linux.cnf.DimacsReader
import gsd.linux.cnf.DimacsReader.{DimacsHeader, DimacsProblem}
import java.io.File
import gsd.linux.{BExprParser, BExpr, KConfigParser, ConcreteKConfig}
import gsd.linux.BExprParser.BExprResult
import gsd.graph.{UndirectedGraph, DirectedGraph}

// TODO maybe move this to a package object
object Projects {

  val linuxBase = "input/2.6.28.6"
  val busyboxBase = "input/busybox-1.17.2"

  val projects = Map(
    "linux" -> LinuxProject,
    "busybox" -> BusyboxProject
  )
}

trait Project {
  val name: String

  def exconfig: ConcreteKConfig
  def bool: BExprResult
  def dimacs: DimacsProblem
  def header: DimacsHeader
  def implg: DirectedGraph[String]
  def mutexg: UndirectedGraph[String]

}
class FileBasedProject(val name: String,
                       val exconfigFile: Option[String] = None,
                       val boolFile: Option[String] = None,
                       val dimacsFile: Option[String] = None,
                       val implgFile: Option[String] = None,
                       val mutexgFile: Option[String] = None) extends Project {

  override lazy val exconfig = exconfigFile match {
    case Some(f) => KConfigParser.parseKConfigFile(f)
    case None => error("exconfig file not specified in project")
  }

  override lazy val bool = boolFile match {
    case Some(f) => BExprParser.parseBExprResult(f)
    case None => error("boolean expressions file not specified in project")
  }

  override lazy val dimacs = dimacsFile match {
    case Some(f) => DimacsReader.readFile(f)
    case None => error("dimacs file not specified in project")
  }

  override lazy val header = dimacsFile match {
    case Some(f) => DimacsReader.readHeaderFile(f)
    case None => error("dimacs file not specified in project")
  }

  override lazy val implg =
    error("implication graph reader not implemented yet (in this project)")

  override lazy val mutexg =
    error("mutex graph reader not implemented yet (in this project)")
}


import Projects._

object BusyboxProject extends FileBasedProject(
  "busybox-1.17.2",
  Some(busyboxBase + ".exconfig"),
  Some(busyboxBase + ".bool"),
  Some(busyboxBase + ".dimacs"),
  Some(busyboxBase + ".implg")
  )

object LinuxProject extends FileBasedProject(
  "linux-2.6.28.6",
  Some("input/2.6.28.6-edited.exconfig"),
  Some(linuxBase + ".bool"),
  Some(linuxBase + ".dimacs"),
  Some(linuxBase + ".implg")
  )
