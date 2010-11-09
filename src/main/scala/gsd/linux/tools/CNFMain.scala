package gsd.linux.tools

import java.io.PrintStream
import util.logging.ConsoleLogger
import gsd.linux.cnf.{ImplBuilder, SATBuilder, DimacsReader}
import gsd.linux.{KConfigParser, TFMTranslation}

trait CNFMain extends ConsoleLogger with Project {

  import gsd.linux.cnf._

  def main(args: Array[String]) {
    log("Parsing Kconfig...")
    val ck = KConfigParser.parseKConfigFile(inFile)

    log("Converting to abstract syntax...")
    val ak = ck.toAbstractKConfig

    log("Translating to tristate...")
    val trans = new TFMTranslation(ak) with ConsoleLogger
    val exprs = trans.translate

    val out = new PrintStream(outFile)
    out.println(exprs.toCNF(trans.idMap).toDimacs(trans.varMap))
    out.close
  }

}

object LinuxCNFMain extends LinuxProject with CNFMain
object BusyboxCNFMain extends BusyboxProject with CNFMain 

trait ImplGraphMain extends ConsoleLogger with Project {

  def main(args: Array[String]) {
    log("Reading Dimacs File...")
    val header = DimacsReader.readHeaderFile(inFile)
    val dimacs = DimacsReader.readFile(inFile)

    log("Initializing SAT solver...")
    val sat = new SATBuilder(dimacs.cnf, dimacs.numVars, header.generated)
                with ImplBuilder with ConsoleLogger

    log("Building implication graph...")
    val g = sat.mkImplicationGraph(header.varMap)

    val out = new PrintStream(outFile)
    out.println(g.toParseString)
    out.close
  }

}

object LinuxImplGraphMain extends LinuxProject with ImplGraphMain
object BusyboxImplGraphMain extends BusyboxProject with ImplGraphMain

