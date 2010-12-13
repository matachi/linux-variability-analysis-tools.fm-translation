package gsd.linux.tools

import util.logging.ConsoleLogger
import java.io.PrintStream

import gsd.linux.cnf.DimacsReader.{DimacsHeader, DimacsProblem}

import org.clapper.argot._
import java.util.Scanner
import gsd.linux.cnf.{MutexBuilder, DimacsReader, ImplBuilder, SATBuilder}

object MutexGraphMain extends ArgotUtil with ConsoleLogger {

  val name = "MutexGraphMain"

  import ArgotConverters._

  val inParam = parser.parameter[String]("in-file",
    "input file containing CNF in dimacs format, stdin if not specified", true)

  val outParam = parser.parameter[String]("out-file",
    "output file for the mutex graph, stdout if not specified", true)

  val genFlag = parser.flag[Boolean](List("g"),
    "do NOT consider variables that end with '_2' as generated")

  def main(args: Array[String]) {

    try {
      parser.parse(args)

      val (header, problem): (DimacsHeader, DimacsProblem) =
        (pOpt.value, inParam.value) match {
          case (Some(_), Some(_)) =>
            parser.usage("Either a project (-p) is specified or input & output parameters are used.")

          case (Some(p), None) => (p.header, p.dimacs)

          case (None, Some(f)) =>
            (DimacsReader.readHeaderFile(f), DimacsReader.readFile(f))

          case (None, None) =>
            log("Using stdin as input...")
            log("Warning: dimacs parsing from stdin is experimental!")
            val scanner = new Scanner(System.in)
            val header = DimacsReader.readHeader(scanner)
            val dimacs = DimacsReader.read(scanner)

            (header, dimacs)
        }

      val output =
        (pOpt.value, outParam.value) match {
          case (Some(p), None) => new PrintStream(p.mutexgFile.get)
          case (None, Some(f)) => new PrintStream(f)
          case _ => System.out
        }

      execute(header, problem, output)
    }
    catch {
      case e: ArgotUsageException => println(e.message)
    }
  }

  def execute(header: DimacsHeader, dimacs: DimacsProblem,
              out: PrintStream) {

    log("Initializing SAT solver...")
    val sat = new SATBuilder(dimacs.cnf, dimacs.numVars, header.generated)
                with MutexBuilder with ConsoleLogger

    log("Building mutex graph...")

    val additional = if (!(genFlag.value.getOrElse(false))) {
      log("[INFO] Considering features that end with _2 as generated...")
      header.varMap filter { case (k,v) => v.endsWith("_2") } map { _._1 }
    } else Nil

    val g = sat.mkMutexGraph(header.varMap, additional)
    out.println(g.toParseString)
  }

}
