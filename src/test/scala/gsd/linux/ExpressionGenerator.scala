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

import org.scalacheck._
import Gen._

/**
 * Trait for generating Expressions for ScalaCheck.
 * 
 * Pretty much copied from Kiama's AST Generator.
 *
 */
trait ExpressionGenerator {

  val alphaCharMap : Map[String, Int] = {
    val result = new scala.collection.mutable.HashMap[String,Int]
    var i = 1
    for (a <- 'A' until ('z' + 1); b <- 'A' until ('z' + 1)) {
      val key = a.toChar + "" + b.toChar
      result += key -> i
      i = i + 1
    }
    Map() ++ result
  }

  val genId : Gen[BId] =
    for (a <- alphaChar; b <- alphaChar)
      yield BId("" + a + b)

  def genAnd(i : Int) : Gen[BAnd] =
    for (l <- genExpr(i); r <- genExpr(i))
      yield BAnd(l, r)

  def genImplies(i : Int) : Gen[BImplies] =
    for (l <- genExpr(i); r <- genExpr(i))
      yield BImplies(l, r)

  def genOr(i : Int) : Gen[BOr] =
    for (l <- genExpr(i); r <- genExpr(i))
      yield BOr(l, r)

  def genNot(i : Int) : Gen[BNot] =
    for (expr <- genExpr(i))
      yield BNot(expr)

  def genOp(i : Int) =
    oneOf( genAnd(i), genOr(i), genNot(i), genImplies(i) )

  implicit def arbExpr : Arbitrary[BExpr] =
    Arbitrary(sized { s => genExpr(s) })

  def genExpr(i : Int) : Gen[BExpr] =
    if (i == 0) genId else genOp(i / 3)
}

