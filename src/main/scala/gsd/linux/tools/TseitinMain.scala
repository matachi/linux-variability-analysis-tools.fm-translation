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

import util.logging.ConsoleLogger
import java.io.PrintStream
import gsd.linux.BExprParser.BExprResult
import gsd.linux.Tseitin.TransformResult

import org.clapper.argot.{ArgotConverters, ArgotUsageException}
import gsd.linux.{BExpr, Tseitin, BExprParser}

object TseitinMain extends ArgotUtil with ConsoleLogger {

  val name = "TseitinMain"

  import ArgotConverters._

  val lineParam = parser.parameter[String](
    "line", "comma separated line numbers of the expression to apply the transformation.", false)

  val inParam = parser.parameter[String](
    "in-file", "input file containing boolean expressions, stdin if not specified.", true)

  val outParam = parser.parameter[String](
    "out-file", "output file to write rewritten boolean expressions, stdout if not specified.", true)

  def main(args: Array[String]) {
    try {
      parser.parse(args)

      val input: BExprResult = inParam.value match {
        case Some(f) =>
          log("Reading boolean expressions from file...")
          BExprParser.parseBExprResult(f)
        case None =>
          log("Using stdin for input...")
          BExprParser.parseBExprResult(new java.util.Scanner(System.in))
      }

      val output = outParam.value match {
        case Some(f) => new PrintStream(f)
        case None => System.out
      }

      // line number is offset
      val lineNums = lineParam.value.get.split(',') map (_.toInt)
      val indices = lineNums map ( _ - 1 -input.ids.size - input.generated.size )

      if (indices exists { i => i < 0 || i >= input.expressions.size })
        sys.error("Line number must be between 1 and %d for the input file.".format(input.expressions.size))

      execute(input, indices, output)
    }
    catch {
      case e: ArgotUsageException => println(e.message)
    }
  }

  def execute(in: BExprResult, indices: Array[Int], out: PrintStream) {
    val prefix = "_T"

    val unchanged = new collection.mutable.ListBuffer[BExpr]
    val change = new collection.mutable.ListBuffer[BExpr]
    for ((e, i) <- in.expressions.zipWithIndex) yield {
      if (indices contains i) change += e
      else unchanged += e
    }

    // Find the last generated number
    val lastGenInt =
      (in.generated filter (_.startsWith(prefix)) map
      (_.substring(prefix.length)) map (_.toInt)).toList.sorted.lastOption match {
        case Some(n) => n
        case None => 0
      }

    log("Last Generated Variable %d".format(lastGenInt))

    // change foreach { e =>
    //   log("Transforming %s".format(e))
    // }

    val TransformResult(transExprs, transGens) =
      Tseitin.transform(change.toList, prefix, lastGenInt + 1)

    log("Created %d new expressions".format(transExprs.size))

    val result =
      BExprResult(in.ids,
                  in.generated ++ transGens,
                  in.genEqs,
                  unchanged.toList ::: transExprs)

    result.write(out)
  }
}