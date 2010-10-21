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

trait Expr

/**
 * TODO eventually transition old BExpr to this class
 */

object B2Expr {
  implicit def toB2ExprList(lst: List[B2Expr]) = new B2ExprList(lst)

  class B2ExprList(lst: List[B2Expr]) {
    def ||(): B2Expr = ((B2False: B2Expr) /: lst){ _ | _ }
  }
}
sealed abstract class B2Expr extends Expr {
  def |(o: B2Expr): B2Expr = B2Or(this, o)
  def &(o: B2Expr): B2Expr = B2And(this, o)
  def iff(o: B2Expr): B2Expr = B2Iff(this, o)
  def implies(o: B2Expr): B2Expr = B2Implies(this, o)

  def unary_!(): B2Expr = B2Not(this)
  def splitConjunctions(): List[B2Expr] = this match {
    case B2And(x,y) => x.splitConjunctions ::: y.splitConjunctions
    case e => List(e)
  }

  lazy val simplify: B2Expr = this
}

trait BinarySimplify extends B2Expr {
  val l, r: B2Expr
  def simp(f: (B2Expr, B2Expr) => B2Expr) = f(l.simplify, r.simplify)
}

case class B2Not(e: B2Expr) extends B2Expr {

  override def toString = "!" + e

  override lazy val simplify = e match {
    case B2Not(f) => f.simplify
    case B2True => B2False
    case B2False => B2True
    case _ => B2Not(e.simplify)
  }
}

case class B2And(l: B2Expr, r: B2Expr) extends B2Expr with BinarySimplify {

  override def toString = "(" + l + " & " + r + ")"

  override lazy val simplify =
    if (l == B2False || r == B2False) B2False
    else simp(B2And)
}

case class B2Or(l: B2Expr, r: B2Expr) extends B2Expr with BinarySimplify {

  override def toString = "(" + l + " | " + r + ")"

  override lazy val simplify =
    if (l == B2True || r == B2True) B2True
    else simp(B2Or)

}

case class B2Iff(l: B2Expr, r: B2Expr) extends B2Expr with BinarySimplify {
  override def toString = "(" + l + " <=> " + r + ")"

  override lazy val simplify = (l.simplify, r.simplify) match {
    case (B2True, _) => r.simplify
    case (_, B2True) => l.simplify
    case (B2False,_) => ((!r).simplify)
    case (_,B2False) => ((!l).simplify)
    case _ => simp(B2Iff)
  }
}

case class B2Implies(l: B2Expr, r: B2Expr) extends B2Expr with BinarySimplify {
  override def toString = "(" + l + " -> " + r + ")"
  override lazy val simplify = simp(B2Implies)
}

case class B2Id(v: String) extends B2Expr {
  override def toString = v
}
case object B2True extends B2Expr {
  override def &(o: B2Expr) = o
  override def toString = "1"
}
case object B2False extends B2Expr {
  override def |(o: B2Expr) = o
  override def toString = "0"
}




