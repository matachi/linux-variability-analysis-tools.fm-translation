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

import kiama.rewriting.Rewriter

/**
 * Type aliases and implicit conversions or CNF and Clauses.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
object CNF {

  type Clause = List[Lit]
  type CNF = Iterable[Clause]

  implicit def toRichClause(c: Clause) = new {
    def toExpression: B2Expr = c.map(_.toExpression).reduceLeft(B2Or)
  }
  implicit def toRichCNF(c: CNF) = new {
    def toExpression: B2Expr = c.map(_.toExpression).reduceLeft(B2And)
  }
  implicit def toCNFBuilder(e: B2Expr) = new CNFBuilder(e)

}

sealed abstract case class Lit(val value: String) {
  def unary_- : Lit
  def toExpression: B2Expr
}
case class PosLit(v: String) extends Lit(v) {
  def unary_- = NegLit(v)
  override def toExpression = B2Id(v)
  override def toString = v
}
case class NegLit(v: String) extends Lit(v) {
  def unary_- = PosLit(v)
  override def toExpression = B2Not(B2Id(v))
  override def toString = "-" + v
}

class CNFBuilder(e: B2Expr) extends CNFRewriter {
  import CNF._

  def toCNF: CNF =
    rewrite(iffRule <* impliesRule <* demorgansRule <* doubleNotRule <*
          notValueRule <* constantsRule)(e).splitConjunctions
        .remove { _ == B2True }
        .flatMap { transform }
        .map { _.mkClause }

  /**
   * Assumes the expression has been converted to CNF form.
   */
  def mkClause : Clause = {
    def _mkList(c: B2Expr) : Clause = c match {
      case B2Or(B2Id(x),y) => PosLit(x) :: _mkList(y)
      case B2Or(B2Not(B2Id(x)),y) => NegLit(x) :: _mkList(y)
      case B2Or(x,y) => _mkList(B2Or(y,x))
      case B2Id(x) => List(PosLit(x))
      case B2Not(B2Id(x)) => List(NegLit(x))
      case _ => error("Expression should be a clause: " + c)
    }
    _mkList(e)
  }
}

trait CNFRewriter extends Rewriter with Tseitin {

  val demorgansRule = reduce {
    rule {
      case B2Not(B2And(x,y)) => B2Or(B2Not(x), B2Not(y))
      case B2Not(B2Or(x,y)) => B2And(B2Not(x), B2Not(y))
    }
  }

  val iffRule = everywheretd {
    rule {
      case B2Iff(x,y) => B2Or(B2Not(x), y) & B2Or(x, B2Not(y))
    }
  }

  val impliesRule = everywheretd {
    rule {
      case B2Implies(x,y) => B2Or(B2Not(x), y)
    }
  }

  val doubleNotRule = innermost {
    rule {
      case B2Not(B2Not(x)) => x
    }
  }

  val notValueRule = everywheretd {
    rule {
      case B2Not(B2True) => B2False
      case B2Not(B2False) => B2True
    }
  }

  val constantsRule = innermost {
    rule {
      case B2Or(B2False,y) => y
      case B2Or(x,B2False) => x
      case B2And(B2False,_) | B2And(_,B2False) => B2False

      case B2And(B2True,y) => y
      case B2And(x,B2True) => x
      case B2Or(B2True,_) | B2Or(_,B2True) => B2True

      case B2Implies(B2True,y) => y
      case B2Implies(B2False,_) => B2True
      case B2Implies(_,B2True) => B2True
      case B2Implies(x,B2False) => BNot(x)
    }
  }

  val factorCNFRule = innermost {
    rule {
      case B2Or(B2And(x1,y),B2And(x2,z)) if x1 == x2 => B2And(x1,B2Or(y,z))
      case B2Or(B2And(x1,y),B2And(z,x2)) if x1 == x2 => B2And(x1,B2Or(y,z))
      case B2Or(B2And(y,x1),B2And(x2,z)) if x1 == x2 => B2And(x1,B2Or(y,z))
      case B2Or(B2And(y,x1),B2And(z,x2)) if x1 == x2 => B2And(x1,B2Or(y,z))

      //FIXME hack to match on multiple nested ors
      case B2Or(B2Or(w,B2And(x1,y)),B2And(x2,z)) if x1 == x2 => B2Or(w,B2And(x1,B2Or(y,z)))
      case B2Or(B2Or(w,B2And(x1,y)),B2And(z,x2)) if x1 == x2 => B2Or(w,B2And(x1,B2Or(y,z)))
      case B2Or(B2Or(w,B2And(y,x1)),B2And(x2,z)) if x1 == x2 => B2Or(w,B2And(x1,B2Or(y,z)))
      case B2Or(B2Or(w,B2And(y,x1)),B2And(z,x2)) if x1 == x2 => B2Or(w,B2And(x1,B2Or(y,z)))
    }
  }


}