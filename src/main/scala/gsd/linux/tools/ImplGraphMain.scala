package gsd.linux.tools

import util.logging.ConsoleLogger
import gsd.linux.cnf.{ImplBuilder, SATBuilder, DimacsReader}
import java.io.PrintStream

import Projects._

object ImplGraphMain extends ConsoleLogger {

  def main(args: Array[String]) {
    val p = projects(args.head)

    log("Reading Dimacs File...")
    val header = DimacsReader.readHeaderFile(p.dimacsFile)
    val dimacs = DimacsReader.readFile(p.dimacsFile)

    log("Initializing SAT solver...")
    val sat = new SATBuilder(dimacs.cnf, dimacs.numVars, header.generated)
                with ImplBuilder with ConsoleLogger


    log("Building implication graph...")
    log("[WARN] Considering features that end with _2 as generated")
    val g = sat.mkImplicationGraph(header.varMap,
              header.varMap filter
                { case (k,v) => v.endsWith("_2") } map { _._1 })

    val out = new PrintStream(p.implgFile)
    out.println(g.toParseString)
    out.close

    log("Writing transitively reduced implication graph...")
    val graphviz = new PrintStream(p.graphvizFile)
    graphviz.println(g.collapseCliques.transitiveReduction.toGraphvizString())
    graphviz.close
  }

}

object TestMain {

  //FIXME bug with scala:cc that doesn't detect implicit object in Conversion scope
  import Conversion._

  def main(args: Array[String]) {
    val parser = new OptParser {
      opt("project")
      opt("dimacs-file")
      opt("implg-out-file")
      opt("graphviz-out-file")

      constraint("project" || "dimacs-file" && "implg-out-file" && "graphviz-out-file")
    }
    val c = parser.parse(args)
    println(c.getAs[String]("project"))
    println("Rest: " + c.rest)
  }
}