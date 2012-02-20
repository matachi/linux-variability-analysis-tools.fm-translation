/*
 * This file is part of the Linux Variability Modeling Tools (LVAT).
 *
 * Copyright (C) 2011 Steven She <shshe@gsd.uwaterloo.ca>
 *
 * LVAT is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * LVAT is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LVAT.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

package gsd.linux.stats

import util.logging.ConsoleLogger
import java.io.PrintStream

import gsd.linux.cnf.DimacsReader.{DimacsHeader, DimacsProblem}

import gsd.linux.cnf.{DimacsReader, SATBuilder}
import java.util.Scanner
import org.clapper.argot._
import gsd.linux.tools.ArgotUtil

object DeadFeaturesMain extends ArgotUtil with ConsoleLogger {

  import ArgotConverters._

  val name = "DeadFeaturesMain"

  val inParam = parser.parameter[String]("in-file",
    "input file containing CNF in dimacs format, stdin if not specified", false)

  val outParam = parser.parameter[String]("out-file",
    "output file for the list of dead features, stdout if not specified", true)

  val genFlag = parser.flag[Boolean](List("g"),
    "do NOT consider variables that end with '_m' as generated")


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
          case (Some(p), None) => new PrintStream(p.implgFile.get)
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

    val generated =
      if (genFlag.value.getOrElse(false)) header.generated
      else {
        log("[INFO] Considering features that end with _m as generated...")
        header.generated ++
          (header.varMap filter { case (_,v) => v.endsWith("_m") } map (_._1))
      }

    log("Initializing SAT solver...")
    val sat = new SATBuilder(dimacs.cnf, dimacs.numVars, generated)
                with ConsoleLogger

    val stats = new SATStatistics(sat, header.idMap) with ConsoleLogger

    stats.deadFeatures foreach out.println

  }

}
