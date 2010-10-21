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

object ExprUtil extends Rewriter {

  val sFixExpr =
    innermost {
      rule {
        case B2Or(B2True, y) => B2True
        case B2Or(x, B2True) => B2True
        case B2Or(B2False, y) => y
        case B2Or(x, B2False) => x
        case B2And(B2True, y) => y
        case B2And(x, B2True) => x
        case B2And(B2False, y) => B2False //TODO I hope this doesn't happen
        case B2And(x, B2False) => B2False //TODO I hope this doesn't happen
        case B2Implies(B2False, y) => B2True
        case B2Implies(B2True, y) => y
        case B2Implies(x,B2False) => !x
        case B2Implies(x,B2True) => B2True
        case B2Not(B2Not(x)) => x
      }
    }

  def removeTrue(lst: List[B2Expr]): List[B2Expr] =
    lst remove { _ == B2True }

  def rewriteExpr(lst: List[B2Expr]): List[B2Expr] =
    rewrite(sFixExpr)(lst)
  
}