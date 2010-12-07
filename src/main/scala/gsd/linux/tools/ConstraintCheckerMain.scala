package gsd.linux.tools

import org.clapper.argot._
import gsd.linux.cnf._
import collection.immutable.PagedSeq
import java.io.{PrintStream, InputStreamReader}
import gsd.linux.{BExpr, BExprParser}

object ConstraintCheckerMain {

  import ArgotConverters._

  val parser = new ArgotParser("ConstraintCheckerMain", preUsage = Some("Check boolean expressions against an input CNF"))

  val cnfParam = parser.parameter[String]("cnf-file",
    "input file containing CNF to check against in dimacs format", false)

  val checkParam = parser.parameter[String]("expr-file",
    "input file containing boolean expressions to check against input CNF", true)

  val outParam = parser.parameter[String]("out-file",
    "output file for results, stdout if not specified", true)

  def main(args: Array[String]) {
    try {
      parser.parse(args)

      val cnfFile = cnfParam.value.get // mandatory parameter
      val (header, problem) =
        (DimacsReader.readHeaderFile(cnfFile), DimacsReader.readFile(cnfFile))
      val sat = new SATBuilder(problem.cnf, problem.numVars, header.generated)

      val exprs = checkParam.value match {
        case Some(f) =>
          BExprParser.parseBExprFile(f).expressions
        case None =>
          println("Reading expression from stdin...")
          BExprParser.parseBExpr(
            PagedSeq fromReader new InputStreamReader(System.in)).expressions
      }

      val out = outParam.value match {
        case Some(f) => new PrintStream(f)
        case None => System.out
      }

      execute(sat, header.idMap, exprs, out)
    }
    catch {
      case e: ArgotUsageException => println(e.message)
    }
  }

  def execute(sat: SATBuilder,
              idMap: Map[String, Int],
              exprs: List[BExpr],
              out: PrintStream) {

    for (e <- exprs) {
      val constraints = sat.addCNF(e.toCNF(idMap))
      out println sat.isSatisfiable
      val results = constraints map sat.removeConstr
      assert (results.forall{ _ == true })
    }

  }
}