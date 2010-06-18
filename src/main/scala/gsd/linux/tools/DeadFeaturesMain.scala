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

import gsd.linux.CNFParser.CNFData
import java.io.PrintStream
import gsd.linux.{PosLit, SATBuilder, IdMapBuilder, CNFParser}

object DeadFeaturesMain {

  /**
   * Parameters:
   *  1. CNF File
   *  2. (Optional) Output File
   */
  def main(args: Array[String]) {
    val out = if (args.size > 1) new PrintStream(args(1)) else System.out
    val CNFData(cnf, ids, gens) = CNFParser.parseCNFFile(args(0))

    val sat = new SATBuilder(IdMapBuilder.mkIdMap(ids ++ gens))
    sat.addCNF(cnf)
    ids.foreach { id =>
      if (!sat.isSatisfiable(PosLit(id))) out.println(id)
    }
  }

}