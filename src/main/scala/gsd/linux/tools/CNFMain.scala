package gsd.linux.tools

import java.io.PrintStream
import util.logging.ConsoleLogger
import gsd.linux.cnf.{ImplBuilder, SATBuilder, DimacsReader}
import gsd.linux.{KConfigParser, TFMTranslation}

import Projects._

object CNFMain extends ConsoleLogger {

  import gsd.linux.cnf._

  def main(args: Array[String]) {
    val p = projects(args.head)

    log("Parsing Kconfig...")
    val ck = KConfigParser.parseKConfigFile(p.exconfigFile)

    log("Converting to abstract syntax...")
    val ak = ck.toAbstractKConfig

    log("Translating to tristate...")
    val trans = new TFMTranslation(ak) with ConsoleLogger
    val exprs = trans.translate

    val out = new PrintStream(p.dimacsFile)
    val idMap = trans.idMap
    println("# of generated: " + trans.generated.size)
    out.println(exprs.toCNF(idMap).toDimacs(trans.varMap, trans.generated.toSet map idMap.apply))
    out.close
  }

}

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


