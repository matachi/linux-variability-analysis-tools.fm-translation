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

package gsd.linux.tools

import gsd.linux._
import gsd.linux.BExprParser.BExprResult

import org.clapper.argot._
import util.logging.ConsoleLogger

object ExpressionAnalysisMain extends ConsoleLogger {

  import ArgotConverters._

  val parser = new ArgotParser("ExpressionAnalysisMain", preUsage = Some(""))

  val exprParam = parser.parameter[String]("expr-file",
    "input file containing boolean expressions", false)

  val thresholdParam = parser.parameter[Int]("threshold",
    "threshold for identifier count", false)

  def main(args: Array[String]) {
    try {
      parser.parse(args)

      val input: BExprResult = exprParam.value match {
        case Some(f) =>
          log("Reading boolean expressions from file...")
          BExprParser.parseBExprResult(f)
        case None =>
          log("Using stdin for input...")
          BExprParser.parseBExprResult(new java.util.Scanner(System.in))
      }

      identifierCount(input, thresholdParam.value.get)
    }
    catch {
      case e: ArgotUsageException => println(e.message)
    }
  }

  def identifierCount(in: BExprResult, threshold: Int) {

    def lineNum(exprNum: Int) =
      in.ids.size + in.generated.size + exprNum + 1

    println {
      in.expressions.zipWithIndex filter { case (e,i) =>
        e.identifiers.size >= threshold
      } map { case (_,i) => lineNum(i) } mkString(",")
    }
  }


}