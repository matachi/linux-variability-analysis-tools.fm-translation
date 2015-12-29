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

import gsd.linux.cnf.SATBuilder
import com.typesafe.scalalogging.LazyLogging

class SATStatistics(val sat: SATBuilder,
                    val idMap: Map[String, Int],
                    val undefined: Set[String] = Set()) extends LazyLogging {

  // Will ignore features that are not present in the idMap
  val undefinedVars: Set[Int] =
    undefined flatMap (i => idMap.get(i).toList)

  val varMap: Map[Int, String] =
    idMap map { case(k,v) => v -> k } toMap

  lazy val deadVars: List[Int] = {
    val result = new collection.mutable.ListBuffer[Int]
    for (i <- (1 to sat.size) if !sat.genVars.contains(i)) {
      if (!sat.isSatisfiable(List(i))) {
        logger.info("Dead feature: %s".format(varMap(i))) // FIXME: accessing idMap in vars
        result += i
      }
    }
    result.toList
  }

  lazy val deadFeatures =
    deadVars map (varMap(_))

  /**
   * Features that are dead because of an undefined feature.
   * Undefined features are included.
   */
  lazy val deadVarsFromUndefinedIncludingUndefined: List[Int] = {
    val result = new collection.mutable.ListBuffer[Int]
    val negUndefinedVars = (undefinedVars map (-_)).toList
    for (i <- (1 to sat.size) if !sat.genVars.contains(i) && !deadVars.contains(i)) {
      // Assume that all undefined vars are negated
      if (!sat.isSatisfiable(i :: negUndefinedVars)) {
        logger.info("Dead feature from undefined: %s".format(varMap(i))) // FIXME: accessing idMap in vars
        result += i
      }
    }
    result.toList
  }

  lazy val deadFeaturesFromUndefinedIncludingUndefined =
    deadVarsFromUndefinedIncludingUndefined map (varMap(_))

  lazy val deadFeaturesFromUndefined =
    deadVarsFromUndefinedIncludingUndefined filterNot (undefinedVars.contains) map (varMap(_))

}

