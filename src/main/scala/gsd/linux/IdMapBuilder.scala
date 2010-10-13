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

import CNF._

object IdMapBuilder {

  def identifiers(c: CNF): Set[String] =
    Set() ++ c.flatMap[String]{ _.map { case e: Lit => e.value }}
  
  def identifiers(e: B2Expr): Set[String] = {
    val rw = new Object with Rewriter
    rw.collects {
      case B2Id(n) => n
    }(e)
  }


  implicit def toRichIdMap[T](m: Map[T,Int]) = new {
    def inverse = Map() ++ m.map { case (k,v) => (v,k) }
  }

  /**
   * Assigns a unique integer for each feature in vs beginning at 1
   */
  def mkIdMap[T](vs: Iterable[T]): Map[T, Int] = {
    var i = 1
    def nextIthVar = {val result = i; i += 1; result}
    Map() ++ vs.map {_ -> nextIthVar}
  }

}