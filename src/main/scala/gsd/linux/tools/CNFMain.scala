package gsd.linux.tools

import util.logging.ConsoleLogger
import Projects._
import org.clapper.argot._
import gsd.linux.BExprParser
import collection.immutable.PagedSeq
import java.io.{InputStreamReader, PrintStream}
import gsd.linux.BExprParser.BExprResult

object CNFMain extends ArgotUtil with ConsoleLogger {

  import gsd.linux.cnf._
  import ArgotConverters._

  val name = "CNFMain"

  val verbose = parser.flag[Boolean](List("v", "verbose"), "enable verbose output")

  val inParam = parser.parameter[String](
    "in-file", "input file containing boolean expressions, stdin if not specified.", true)

  val outParam = parser.parameter[String](
    "out-file", "output file to write CNF in dimacs format, stdout if not specified.", true)


  def main(args: Array[String]) {
    try {
      parser.parse(args)

      val input =
        (pOpt.value, inParam.value) match {
          case (Some(_), Some(_)) =>
            parser.usage("Either a project (-p) is specified or input & output parameters are used.")

          case (None, Some(f)) =>
            log("Reading boolean expressions from file...")
            BExprParser.parseBExprResult(f)

          case (Some(p), None) => p.bool

          case (None, None) =>
            log("Using stdin for input...")
            BExprParser.parseBExprResult(new java.util.Scanner(System.in))
        }

      val output =
        (pOpt.value, outParam.value) match {
          case (None, Some(f)) => new PrintStream(f)
          case (Some(p), None) => new PrintStream(p.dimacsFile.get)
          case _ => System.out
        }

      execute(input, verbose.value.getOrElse(false), output)
    }
    catch {
      case e: ArgotUsageException => println(e.message)
    }
  }

  def execute(in: BExprResult, verbose: Boolean, out: PrintStream) {
    log("# of generated: " + in.generated.size)

    val cnf = in.expressions flatMap { e =>
      if (verbose) log("Converting: " + e)
      e.toCNF(in.idMap)
    }

    out.println(cnf.toDimacs(in.varMap, in.generated.toSet map in.idMap.apply))
  }

}



