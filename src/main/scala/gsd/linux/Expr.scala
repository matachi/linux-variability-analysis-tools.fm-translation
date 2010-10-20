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

trait Expr {
  val isTerminal: Boolean = false
}
trait Terminal extends Expr {
  override val isTerminal: Boolean = true
}

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
}
case class B2Not(e: B2Expr) extends B2Expr with Terminal
case class B2And(l: B2Expr, r: B2Expr) extends B2Expr
case class B2Or(l: B2Expr, r: B2Expr) extends B2Expr
case class B2Iff(l: B2Expr, r: B2Expr) extends B2Expr
case class B2Implies(l: B2Expr, r: B2Expr) extends B2Expr

case class B2Id(v: String) extends B2Expr with Terminal

case object B2True extends B2Expr with Terminal {
  override def &(o: B2Expr) = o
}
case object B2False extends B2Expr with Terminal {
  override def |(o: B2Expr) = o
}




