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

import org.kiama.rewriting.Rewriter._

/**
 * Type aliases and implicit conversions or CNF and Clauses.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
object CNF {

  type Clause = List[Lit]
  type CNF = Iterable[Clause]

  implicit def toRichClause(c: Clause) = new {
    def toExpression: BExpr = c.map(_.toExpression).reduceLeft(BOr)
  }
  implicit def toRichCNF(c: CNF) = new {
    def toExpression: BExpr = c.map(_.toExpression).reduceLeft(BAnd)
  }
  implicit def toCNFBuilder(e: BExpr) = new CNFBuilder(e)

}

object Lit {
  def unapply(l: Lit): Option[String] = Some(l.value)
}
sealed abstract class Lit(val value: String) {
  def unary_- : Lit
  def toExpression: BExpr
}
case class PosLit(v: String) extends Lit(v) {
  def unary_- = NegLit(v)
  override def toExpression = BId(v)
  override def toString = v
}
case class NegLit(v: String) extends Lit(v) {
  def unary_- = PosLit(v)
  override def toExpression = BNot(BId(v))
  override def toString = "-" + v
}

class CNFBuilder(e: BExpr) extends CNFRewriter {
  import CNF._

  def toCNF: CNF =
    rewrite(iffRule <* impliesRule <* demorgansRule <* doubleNotRule <*
          notValueRule <* constantsRule)(e).splitConjunctions
        .filterNot { _ == BTrue }
        .flatMap { transform }
        .map { _.mkClause }

  /**
   * Assumes the expression has been converted to CNF form.
   */
  def mkClause : Clause = {
    def _mkList(c: BExpr) : Clause = c match {
      case BOr(BId(x),y) => PosLit(x) :: _mkList(y)
      case BOr(BNot(BId(x)),y) => NegLit(x) :: _mkList(y)
      case BOr(x,y) => _mkList(BOr(y,x))
      case BId(x) => List(PosLit(x))
      case BNot(BId(x)) => List(NegLit(x))
      case _ => error("Expression should be a clause: " + c)
    }
    _mkList(e)
  }
}

trait CNFRewriter extends Tseitin {

  val demorgansRule = reduce {
    rule {
      case BNot(BAnd(x,y)) => BOr(BNot(x), BNot(y))
      case BNot(BOr(x,y)) => BAnd(BNot(x), BNot(y))
    }
  }

  val iffRule = everywheretd {
    rule {
      case BIff(x,y) => BOr(BNot(x), y) & BOr(x, BNot(y))
    }
  }

  val impliesRule = everywheretd {
    rule {
      case BImplies(x,y) => BOr(BNot(x), y)
    }
  }

  val doubleNotRule = innermost {
    rule {
      case BNot(BNot(x)) => x
    }
  }

  val notValueRule = everywheretd {
    rule {
      case BNot(BTrue) => BFalse
      case BNot(BFalse) => BTrue
    }
  }

  val constantsRule = innermost {
    rule {
      case BOr(BFalse,y) => y
      case BOr(x,BFalse) => x
      case BAnd(BFalse,_) | BAnd(_,BFalse) => BFalse

      case BAnd(BTrue,y) => y
      case BAnd(x,BTrue) => x
      case BOr(BTrue,_) | BOr(_,BTrue) => BTrue

      case BImplies(BTrue,y) => y
      case BImplies(BFalse,_) => BTrue
      case BImplies(_,BTrue) => BTrue
      case BImplies(x,BFalse) => BNot(x)
    }
  }

  val factorCNFRule = innermost {
    rule {
      case BOr(BAnd(x1,y),BAnd(x2,z)) if x1 == x2 => BAnd(x1,BOr(y,z))
      case BOr(BAnd(x1,y),BAnd(z,x2)) if x1 == x2 => BAnd(x1,BOr(y,z))
      case BOr(BAnd(y,x1),BAnd(x2,z)) if x1 == x2 => BAnd(x1,BOr(y,z))
      case BOr(BAnd(y,x1),BAnd(z,x2)) if x1 == x2 => BAnd(x1,BOr(y,z))

      //FIXME hack to match on multiple nested ors
      case BOr(BOr(w,BAnd(x1,y)),BAnd(x2,z)) if x1 == x2 => BOr(w,BAnd(x1,BOr(y,z)))
      case BOr(BOr(w,BAnd(x1,y)),BAnd(z,x2)) if x1 == x2 => BOr(w,BAnd(x1,BOr(y,z)))
      case BOr(BOr(w,BAnd(y,x1)),BAnd(x2,z)) if x1 == x2 => BOr(w,BAnd(x1,BOr(y,z)))
      case BOr(BOr(w,BAnd(y,x1)),BAnd(z,x2)) if x1 == x2 => BOr(w,BAnd(x1,BOr(y,z)))
    }
  }


}