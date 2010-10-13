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

package gsd.linux

/**
 * A simple Tseitin's Transformation that introduces a new variable for each
 * sub-expression.
 *
 * An improved version should eventually replace the manual transformation done
 * in the LVAT BooleanTranslation.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
trait Tseitin {

  object IdGen {
     var i = 0
     def next = { i+=1; "x" + i }
     def allIds = (1 to i).map { "x" + _ }.toList
   }

  /**
   * Assumes (bi-)implications have been factored out.
   */
  def transform(in: B2Expr): List[B2Expr] = {
    def _tt(e: B2Expr): Pair[B2Id, List[B2Expr]] = {
      val eId = B2Id(IdGen.next)
      e match {
        case n: B2Id => (eId, List(!eId | n, eId | !n))
        case B2Not(x) => {
          val (xId, xExprs) = _tt(x)
          eId -> ((!eId | !xId) :: (eId | xId) :: xExprs)
        }
        case B2And(x,y) => {
          val (xId, xExprs) = _tt(x)
          val (yId, yExprs) = _tt(y)
          eId -> ((!eId | xId) :: (!eId | yId) :: (eId | !xId | !yId) :: xExprs ::: yExprs)
        }
        case B2Or(x,y) => {
          val (xId, xExprs) = _tt(x)
          val (yId, yExprs) = _tt(y)
          eId -> ((eId | !xId) :: (eId | !yId) :: (!eId | xId | yId) :: xExprs ::: yExprs)
        }
        case _ => error("not supported: " + e + " from: " + in)
      }
    }
    val (ttId, ttExprs) = _tt(in)
    ttId :: ttExprs
  }


}
