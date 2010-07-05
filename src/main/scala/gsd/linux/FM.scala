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

case class FM[T <: Expr](features: List[Node[T]])

sealed abstract class Node[T <: Expr]
  (val children: List[Node[T]])

//TODO parameterize TExpr so that we can substitute with BExpr
sealed abstract class Feature[T <: Expr]
  (val name: String, ftype: FeatType,
   constraints: List[T], cs: List[Node[T]]) extends Node[T](cs)

case class OFeature[T <: Expr]
  (n: String, t: FeatType,
   ctcs: List[T], cs: List[Node[T]]) extends Feature[T](n, t, ctcs, cs)

case class MFeature[T <: Expr]
  (n: String, t: FeatType,
   ctcs: List[T], cs: List[Node[T]]) extends Feature[T](n, t, ctcs, cs)

sealed abstract class Group[T <: Expr]
  (val members: List[Node[T]],
   val constraints: List[T]) extends Node[T](members)

case class OrGroup[T <: Expr]
  (mems: List[Node[T]],
   ctcs: List[T]) extends Group[T](mems, ctcs)

case class XorGroup[T <: Expr]
  (mems: List[Node[T]],
   ctcs: List[T]) extends Group[T](mems, ctcs)

sealed abstract class FeatType
case object BoolFeat extends FeatType
case object TriFeat extends FeatType
case object IntFeat extends FeatType
case object StringFeat extends FeatType

import Document._

//FIXME hard-coded for boolean format
trait FMDocument extends TExprDocument with B2ExprDocument {

  def toText[T <: Expr](f: Node[T]): Text = f match {
    case OFeature(name,t,ctcs,cs) =>
      name :: "?" :: 
              Block("", "", cs map toText[T]) :/:
              Block("[", "]", iterToText(ctcs map toText[T])(_ :/: _)) :: NL
    
    case MFeature(name,t,ctcs,cs) =>
      name :: ":" :: Block("", "", cs map toText[T]) :: NL

    case _ =>
      //TODO
      error("Unsupported (should only be groups here)!")
  }

  def toText[T <: Expr](e: T): Text = e match {
    case t: TExpr => toExprText(t)
    case b: B2Expr => toExprText(b)
  }

  def toText[T <: Expr](fm: FM[T]): Text =
    Block("fm {", "}", fm.features map toText[T])

}

trait TExprDocument {

  def toExprText(e: TExpr): Text = {
    def _paren(e: TExpr): Text =
      if (e.isTerminal || e.getClass == this.getClass) toExprText(e) //FIXME
      else "(" +: toExprText(e) +: ")"

    e match {
      case TYes => "y"
      case TMod => "m"
      case TNo => "n"
      case TAnd(x,y) => _paren(x) :: "&" :: _paren(y)
      case TOr(x,y) => _paren(x) :: "|" :: _paren(y)
      case TEq(x,y) => _paren(x) :: "=" :: _paren(y)
      case TGte(x,y) => _paren(x) :: ">=" :: _paren(y)
      case TLte(x,y) => _paren(x) :: "<=" :: _paren(y)

      case TNot(x) => "!" +: toExprText(x)
      case TBool(x) => "Bool(" +: toExprText(x) +: string(")")
      case TId(x) => string(x)

      case TImplies(x,y) => _paren(x) :: "->" :: _paren(y)

      case _ => StringT(e.toString)
    }
  }

}

trait B2ExprDocument {

  def toExprText(e: B2Expr): Text = {
    def _paren(e: B2Expr): Text =
      if (e.isTerminal || e.getClass == this.getClass) toExprText(e) //FIXME
      else "(" +: toExprText(e) +: ")"

    e match {
      case B2True => "1"
      case B2False => "0"
      case B2And(x,y) => _paren(x) :: "&" :: _paren(y)
      case B2Or(x,y) => _paren(x) :: "|" :: _paren(y)
      case B2Implies(x,y) => _paren(x) :: "->" :: _paren(y)
      case B2Iff(x,y) => _paren(x) :: "<->" :: _paren(y)

      case B2Not(x) => "!" +: toExprText(x)
      case B2Id(x) => string(x)

      case _ => StringT(e.toString)
    }
  }

}

