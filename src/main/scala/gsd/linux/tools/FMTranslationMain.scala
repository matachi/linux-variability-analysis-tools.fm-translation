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

import java.io.PrintStream
import gsd.linux._
import gsd.linux.CNFParser.CNFData

object FMTranslationMain {

  /**
   * Parameters:
   *  1. KConfig Extract File
   *  2. CNF Expression File
   *  3. (Optional) Output file
   */
  def main(args: Array[String]): Unit = {
    val out = if (args.size > 2) new PrintStream(args(2)) else System.out

    val CNFData(cnf, ids, gens) = CNFParser.parseCNFFile(args(1))
    val idMap = IdMapBuilder.mkIdMap(ids ++ gens)
    val ck = KConfigParser.parseKConfigFile(args(0))
    val parentMap = Hierarchy.mkParentMap(ck)

    println("Loading SAT Solver...")
    val sat = new SATBuilder(idMap)
    sat.addCNF(cnf)

    FMTranslation2.mkFeatureModel(sat, ck)
  }
  
}