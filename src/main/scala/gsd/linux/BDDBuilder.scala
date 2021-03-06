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
import cnf._

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
  }
}

class BDDBuilder(val idMap: Map[String, Int]) {
  
  val varMap = (idMap map { case (k,v) => (v,k) }).toMap

  val factory = BDDFactory.init("java", 15000000, 1000000)
  factory.setVarNum(idMap.keySet.size * 2)

  implicit def toBDDExpr(e: BExpr) = new {
    def mkBDD: BDD = BDDBuilder.this.mkBDD(e)
  }

  def apply(e: BExpr): BDD = mkBDD(e)

  def mkBDD(in: Clause): BDD =
    (in map {
      case i if i < 0 => factory nithVar -i
      case i => factory ithVar i
    }).foldLeft(zero){ _ orWith _ }

  def mkBDD(in: CNF): BDD =
    (in map mkBDD).foldLeft(one){ _ andWith _ }

  def mkBDD(in: BExpr): BDD =
    mkBDDWithConjunctions(in.splitConjunctions)

  def mkBDDWithConjunctions(lst: List[BExpr]) = {
    def _mkBDD(e: BExpr) : BDD = e match {
      case BAnd(x,y) => _mkBDD(x) andWith _mkBDD(y)
      case BOr(x,y) => _mkBDD(x) orWith _mkBDD(y)
      case BImplies(x,y) => _mkBDD(x) impWith _mkBDD(y)
      case BNot(x) =>
        val temp = _mkBDD(x)
        BDDBuilder.begin0(temp.not)(temp.free)
      case BId(x) => factory ithVar idMap(x)
      case BTrue  => factory one
      case BFalse => factory zero
      case _ => sys.error("This case should never occur.")
    }
    (one /: lst.map(_mkBDD)){_ andWith _}
  }

  def one  = factory one
  def zero = factory zero
  def close = factory.done

  def ithVar(s: String): BDD =
    factory ithVar idMap(s)

  def reset = {
    factory.reset
    factory.setVarNum(idMap.keySet.size * 2)
  }

  /**
   * Converts a BDD representating a satisfying assignment to CNF form.
   */
  def toCNF(sat: BDD): CNF = 
    sat.scanSet.map { v => List(v) } ++
            (Set() ++ idMap.values -- sat.scanSet).map { v => List(-v) }
}
