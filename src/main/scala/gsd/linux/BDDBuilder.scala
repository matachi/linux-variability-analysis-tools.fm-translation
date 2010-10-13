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

import net.sf.javabdd._
import CNF._
import IdMapBuilder._

object BDDBuilder {
  def begin0[T](thunk : => T)(after : => Unit) : T = {
    val result = thunk
    after
    result
  }

  /**
   * Convenience methods for dealing with lists of BDDs.
   */
  implicit def toBDDList(lst : List[BDD]) = new {
    def freeAfter[T](thunk : => T) : T = {
      val result = thunk
      for (bdd <- lst) bdd.free
      result
    }
    def orWithAll: BDD =  lst reduceLeft { _ orWith _ }
    def andWithAll: BDD =  lst reduceLeft { _ andWith _ }
  }
}

class BDDBuilder(val idMap: Map[String, Int]) {
  val varMap = idMap.inverse

  val factory = BDDFactory.init("java", 15000000, 1000000)
  factory.setVarNum(idMap.keySet.size * 2)

  implicit def toBDDExpr(e: B2Expr) = new {
    def mkBDD: BDD = BDDBuilder.this.mkBDD(e)
  }

  def apply(e: B2Expr): BDD = mkBDD(e)

  def mkBDD(in: B2Expr): BDD =
    mkBDDWithConjunctions(in.splitConjunctions)

  def mkBDDWithConjunctions(lst: List[B2Expr]) = {
    def _mkBDD(e: B2Expr) : BDD = e match {
      case B2And(x,y) => _mkBDD(x) andWith _mkBDD(y)
      case B2Or(x,y) => _mkBDD(x) orWith _mkBDD(y)
      case B2Implies(x,y) => _mkBDD(x) impWith _mkBDD(y)
      case B2Not(x) =>
        val temp = _mkBDD(x)
        BDDBuilder.begin0(temp.not)(temp.free)
      case B2Id(x) => factory ithVar idMap(x)
      case B2True  => factory one
      case B2False => factory zero
      case _ => error("This case should never occur.")
    }
    (one /: lst.map(_mkBDD)){_ andWith _}
  }

  def one  = factory one
  def zero = factory zero
  def close = factory.done

  def reset = {
    factory.reset
    factory.setVarNum(idMap.keySet.size * 2)
  }

  /**
   * Converts a BDD representating a satisfying assignment to CNF form.
   */
  def toCNF(sat: BDD): CNF = 
    sat.scanSet.map { v => List(PosLit(varMap(v))) } ++
            (Set() ++ idMap.values -- sat.scanSet).map { v => List(NegLit(varMap(v))) }
}
