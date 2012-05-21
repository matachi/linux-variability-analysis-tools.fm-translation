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

import org.kiama.rewriting.Rewriter


/**
 * A simple Tseitin's Transformation that introduces a new variable for each
 * sub-expression.
 *
 * An improved version should eventually replace the manual transformation done
 * in the LVAT BooleanTranslation.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
object Tseitin {

  case class TransformResult(expressions: List[BExpr], generated: List[String])

  case class IdGen(start: Int, prefix: String) {
    var i = start
    def next = { i+=1; prefix + i }
    def allIds = (start to i).map { prefix + _ }.toList
  }

  def transform(in: List[BExpr], prefix: String = "_T", offset: Int = 0): TransformResult = {
    val idGen = IdGen(offset, prefix)

    def _transform(ein: BExpr): List[BExpr] = {
      import gsd.linux.cnf.CNFBuilder._
      val e = Rewriter.rewrite(sIffRule <* sImpliesRule)(ein).simplify

      def _tt(e: BExpr): Pair[BId, List[BExpr]] = {
        e match {
          case v: BId => {
            val eId = BId(idGen.next)
            eId -> List((!eId | v), (eId | !v))
          }
          case BNot(x) => {
            val (xId, xExprs) = _tt(x)
            val eId = BId(idGen.next)
            eId -> ((!eId | !xId) :: (eId | xId) :: xExprs)
          }
          case BAnd(x,y) => {
            val (xId, xExprs) = _tt(x)
            val (yId, yExprs) = _tt(y)
            val eId = BId(idGen.next)
            eId -> ((!eId | xId) :: (!eId | yId) :: (eId | !xId | !yId) :: xExprs ::: yExprs)
          }
          case BOr(x,y) => {
            val (xId, xExprs) = _tt(x)
            val (yId, yExprs) = _tt(y)
            val eId = BId(idGen.next)
            eId -> ((eId | !xId) :: (eId | !yId) :: (!eId | xId | yId) :: xExprs ::: yExprs)
          }
          case _ => sys.error("not supported: " + e + " from: " + in)
        }
      }

      val (ttId, ttExprs) = _tt(e)
      ttId :: ttExprs
    }

    val results = in map _transform
    TransformResult(results reduceLeft (_ ::: _), idGen.allIds)
  }

}

