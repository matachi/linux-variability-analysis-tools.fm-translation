/*
 * This file is part of the Linux Variability Modeling Tools (LVAT).
 *
 * Copyright (C) 2010 Steven She <shshe@gsd.uwaterloo.ca>
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

import java.io.PrintStream
import org.clapper.argot.{ArgotConverters, ArgotUsageException}
import com.typesafe.scalalogging.LazyLogging

/**
 * Outputs the boolean translation of a Kconfig extract. 
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
object TristateTranslationMain extends ArgotUtil with LazyLogging {

  val name = "TristateTranslationMain"

  import ArgotConverters._

  val inParam = parser.parameter[String](
    "in-file", "input Kconfig extract (.exconfig) file, stdin if not specified.", true)

  val outParam = parser.parameter[String](
    "out-file", "output file to write boolean expressions, stdout if not specified.", true)

  val noUndefinedFlag = parser.flag[Boolean](List("no-undefined"),
    "do NOT add constraints for undefined configs")

  val envParams = parser.multiOption[String](List("env"), "configname",
    "configs that have their values based on environment variables")

  def main(args: Array[String]) {

    try {
      parser.parse(args)

      val k =
      (pOpt.value, inParam.value) match {
        case (Some(_), Some(_)) =>
          parser.usage("Either a project (-p) is specified or input & output parameters are used.")

        case (Some(p), None) => p.exconfig

        case (None, Some(f)) =>
          logger.info("Reading Kconfig extract from file...")
          KConfigParser.parseKConfigFile(f)

        case (None, None) =>
          logger.info("Using stdin as input...")
          KConfigParser.parseKConfigStream(System.in)
      }

      val output =
      (pOpt.value, outParam.value) match {
        case (Some(p), None) => new PrintStream(p.boolFile.get)
        case (None, Some(f)) => new PrintStream(f)
        case _ => System.out
      }

      execute(k, output)
    }
    catch {
      case e: ArgotUsageException => println(e.message)
    }
  }

  def execute(k: ConcreteKConfig, out: PrintStream) {

    val ids = (k.identifiers map BExprUtil.sanitizeString).toSet

    assert(ids.size == k.identifiers.size)

    // TODO refactor this to a propositional formula class
    // First output identifiers
    for (id <- ids) {
      out.println("@ " + id)
      out.println("@ " + id + "_m")
    }

    var ak = k.toAbstractKConfig

    val additionalEnvs = envParams.value
    if (!additionalEnvs.isEmpty)
      ak = ak.copy(env = ak.env ::: additionalEnvs.toList)

    val addUndefined = !noUndefinedFlag.value.getOrElse(false)
    if (!addUndefined)
      logger.info("Not adding constraints for undefined configs")

    val trans = new TristateTranslation(ak, addUndefined)
    val exprs = trans.translate map (BExprUtil.sanitizeExpr)

    for (id <- trans.generated) out.println("$ " + id)

    // TODO move this to a BExprResult
    // variable configname value
    for ( ((name, value), id) <- trans.literalMap.toList sortBy { case ((x,_),_) => x }) 
      value match {
        case Literal(l) =>
          out.println("""$s %s %s %s""".format(id, name, l))
        case Id(x) =>
          out.println("""$v %s %s %s""".format(id, name, x))
        case KInt(i) =>
          out.println("""$i %s %s %s""".format(id, name, i))
        case KHex(h) =>
          out.println("""$h %s %s %s""".format(id, name, h))
        case e =>
          sys.error("Unsupported generated equality: " + e)
      }

    for (e  <- exprs) out.println(e)
  }
}