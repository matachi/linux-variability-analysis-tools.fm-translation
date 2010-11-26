package gsd.linux.tools

import util.logging.ConsoleLogger
import gsd.linux.cnf.{ImplBuilder, SATBuilder}
import java.io.PrintStream

import gsd.linux.cnf.DimacsReader.{DimacsHeader, DimacsProblem}
import gsd.graph.DirectedGraph

import org.clapper.argot._

object ImplGraphMain extends ConsoleLogger {

  import ArgotConverters._

  val parser = new ArgotParser("ImplGraphMain")
  val pOpt = parser.option[String](
    List("p", "project"), "p", "supported projects: linux, busybox")
  val dimacsOpt = parser.option[String](
    "dimacs-file", "f", "file containing CNF in dimacs format")
  val implgOpt = parser.option[String](
    "implg-file", "f", "output file for the implication graph")

  def main(args: Array[String]) {
    import Projects._

    try {
      parser.parse(args)
    }
    catch {
      case e: ArgotUsageException => println(e.message)
    }

    // Is there a project parameter?
    pOpt.value match {
      case Some(ps) if projects contains ps =>
        val p = projects(ps)
        execute(p.dimacs, p.header, p.implgFile)

      case Some(p) =>
        exit("Project not found, valid values are: " + projects.keySet.mkString(","))

      case None if dimacsOpt.value.isDefined =>
        val p = new FileBasedProject("cmd-line",
          dimacsFile = dimacsOpt.value,
          implgFile = implgOpt.value)
        execute(p.dimacs, p.header, p.implgFile)

      case _ =>
        exit("Either --project or --dimacs-file must be defined.")
    }
  }

  def exit(msg: String) {
    parser.usage(msg)
    System.exit(1)
  }

  def execute(dimacs: DimacsProblem, header: DimacsHeader,
                  outFile: Option[String]) {
    
    log("Initializing SAT solver...")
    val sat = new SATBuilder(dimacs.cnf, dimacs.numVars, header.generated)
                with ImplBuilder with ConsoleLogger

    log("Building implication graph...")
    log("[WARN] Considering features that end with _2 as generated")
    val g = sat.mkImplicationGraph(header.varMap,
              header.varMap filter
                { case (k,v) => v.endsWith("_2") } map { _._1 })

    val out = outFile match {
      case Some(f) =>
        log("Saving implication graph to %s...".format(f))
        new PrintStream(f)
      case None => System.out
    }

    out.println(g.toParseString)
  }

}
