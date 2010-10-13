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




object TExpr {
  implicit def toTExprList(lst: List[TExpr]): TExprList = new TExprList(lst)
  implicit def toKExprList(lst: List[KExpr]) = new {
    def toTExprs(): List[TExpr] = lst map toTExpr
  }

  class TExprList(lst: List[TExpr]){
    def ||(): TExpr = ((TNo: TExpr) /: lst){ _ | _ }
    def &&(): TExpr = ((TYes: TExpr) /: lst){ _ & _ }
  }

  implicit def toTExpr(e: KExpr): TExpr = e match {
    case And(l,r) => TAnd(l,r)
    case Or(l,r) => TOr(l,r)
    case Eq(l,r) => TEq(l,r)
    case NEq(l,r) => TNEq(l,r)
    case Not(e) => TNot(e)
    case Id(v) => TId(v)
    case Literal(v) => TLiteral(v)
    case KInt(v) => TInt(v)
    case KHex(v) => TInt(-99999) //FIXME
    case Yes => TYes
    case Mod => TMod
    case No => TNo
  }
}

/**
 * Extended KExpr with support for operators not defined in Kconfig.
 */
sealed abstract class TExpr extends Expr {
  def &(o: TExpr): TExpr = TAnd(this, o)
  def |(o: TExpr): TExpr = TOr(this, o)
  def <(o: TExpr): TExpr = TLt(this, o)
  def >(o: TExpr): TExpr = TGt(this, o)
  def <=(o: TExpr): TExpr = TLte(this, o)
  def >=(o: TExpr): TExpr = TGte(this, o)
  def eqs(o: TExpr): TExpr = TEq(this, o)
  def implies(o: TExpr): TExpr = TImplies(this, o)
  def unary_!(): TExpr = TNot(this)
}

case class TNot(e: TExpr) extends TExpr with Terminal
case class TAnd(l: TExpr, r: TExpr) extends TExpr
case class TOr(l: TExpr, r: TExpr) extends TExpr
case class TEq(l: TExpr, r: TExpr) extends TExpr
case class TNEq(l: TExpr, r: TExpr) extends TExpr

case class TImplies(l: TExpr, r: TExpr) extends TExpr

case class TGt(l: TExpr, r: TExpr) extends TExpr
case class TGte(l: TExpr, r: TExpr) extends TExpr
case class TLt(l: TExpr, r: TExpr) extends TExpr
case class TLte(l: TExpr, r: TExpr) extends TExpr

case class TId(value: String) extends TExpr with Terminal
case class TLiteral(value: String) extends TExpr with Terminal
case class TInt(value: Int) extends TExpr with Terminal
case object TYes extends TExpr with Terminal {
  override def &(o: TExpr): TExpr = o
}
case object TMod extends TExpr with Terminal
case object TNo extends TExpr with Terminal {
  override def |(o: TExpr): TExpr = o
}

/**
 * Helper function for return TYes if v > 0, TNo otherwise.
 * Considered a Terminal because the function provides brackets.
 */
case class TBool(v: TExpr) extends TExpr with Terminal

