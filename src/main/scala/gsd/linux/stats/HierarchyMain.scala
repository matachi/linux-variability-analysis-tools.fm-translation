package gsd.linux.stats

import java.io.PrintStream
import gsd.linux.cnf.{SATBuilder, DimacsReader}
import gsd.linux.{HierarchyAnalysis, KConfigParser}

object HierarchyMain {

  def main(args: Array[String]) {
    if (args.size < 2) {
      System.err.println("Parameters: <exconfig file> <dimacs file> <output file>")
      System exit 1
    }

    val out: PrintStream =
      if (args.size > 2) new PrintStream(args(2))
      else System.out

    println("Reading extract...")
    val k = KConfigParser.parseKConfigFile(args(0))

    println("Reading dimacs...")
    val header = DimacsReader.readHeaderFile(args(1))
    val problem = DimacsReader.readFile(args(1))

    val idMap = header.idMap

    println("Initializing SAT solver...")
    val sat = new SATBuilder(problem.cnf, problem.numVars, header.generated)

    println("Finding hierarchy violating configs...")
    val violating = HierarchyAnalysis.findViolatingConfigs(k, sat, idMap)

    violating foreach { c =>
      out.println(c.id)
    }
  }


}