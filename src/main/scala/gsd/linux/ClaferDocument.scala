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

import Document._

trait ClaferTransforms {
  def fix(s: String): String =
    if (s == "no") "cNo" // reserved word
    else if (s.matches("""^[0-9].*""")) fix("c" + s)
    else s.replaceAll("""[^a-zA-Z0-9_]""", "_")
}

//FIXME hard-coded for boolean format
trait ClaferDocument extends B2ExprDocument {

  def crossTree(ctcs: List[B2Expr]) =
    if (ctcs.isEmpty) NL
    else NL +: Block("[", "]", iterToText(ctcs map toText)(_ :/: _)) +: NL

  def toText(f: Node[B2Expr]): Text = f match {
    case OFeature(name,t,ctcs,cs) =>
      fix(name) +: "?" ::  Block("{", "}", cs map toText) :: crossTree(ctcs)

    case MFeature(name,t,ctcs,cs) =>
      fix(name) :: Block("{", "}", cs map toText) :: crossTree(ctcs)

    case OptGroup(name, cs, ctcs) =>
      "opt" :: fix(name) :: Block("{", "}", cs map toText) :: crossTree(ctcs)

    case OrGroup(name, cs, ctcs) =>
      "or" :: fix(name) :: Block("{", "}", cs map toText) :: crossTree(ctcs)

    case XorGroup(name, cs, ctcs) =>
      "xor" :: fix(name) :: Block("{", "}", cs map toText) :: crossTree(ctcs)

    case MutexGroup(name, cs, ctcs) =>
      "mux" :: fix(name) :: Block("{", "}", cs map toText) :: crossTree(ctcs)
  }

  def toText(e: B2Expr): Text = toExprText(e)

  implicit def toText(fm: FM[B2Expr]): Text =
    toText(fm.root)

}

trait B2ExprDocument extends ClaferTransforms {

  def toExprText(e: B2Expr): Text = {
    def _paren(e: B2Expr): Text =
      "(" +: toExprText(e) +: ")"

    e match {
      case B2True => "1"  //FIXME Clafer doesn't have true
      case B2False => "0" //FIXME Clafer doesn't have false
      case B2And(x,y) => _paren(x) :: "&&" :: _paren(y)
      case B2Or(x,y) => _paren(x) :: "||" :: _paren(y)
      case B2Implies(x,y) => _paren(x) :: "=>" :: _paren(y)
      case B2Iff(x,y) => _paren(x) :: "<=>" :: _paren(y)

      case B2Not(x) => "~" +: toExprText(x)
      case B2Id(x) => string(fix(x))

      case _ => StringT(e.toString)
    }
  }

}


