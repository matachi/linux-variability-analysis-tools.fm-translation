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
  (val children: List[Node[T]], val constraints: List[T])

sealed abstract class Feature[T <: Expr]
  (val name: String, ftype: FeatType,
   cons: List[T], cs: List[Node[T]]) extends Node[T](cs, cons)

case class OFeature[T <: Expr]
  (n: String, t: FeatType,
   ctcs: List[T], cs: List[Node[T]]) extends Feature[T](n, t, ctcs, cs)

case class MFeature[T <: Expr]
  (n: String, t: FeatType,
   ctcs: List[T], cs: List[Node[T]]) extends Feature[T](n, t, ctcs, cs)

sealed abstract class Group[T <: Expr]
  (val name: String,
   val members: List[Node[T]],
   cons: List[T]) extends Node[T](members, cons)

case class OrGroup[T <: Expr]
  (n: String,
   mems: List[Node[T]],
   ctcs: List[T]) extends Group[T](n, mems, ctcs)

case class XorGroup[T <: Expr]
  (n: String,
   mems: List[Node[T]],
   ctcs: List[T]) extends Group[T](n, mems, ctcs)

case class MutexGroup[T <: Expr]
  (n: String,
   mems: List[Node[T]],
   ctcs: List[T]) extends Group[T](n, mems, ctcs)

case class OptGroup[T <: Expr]
  (n: String,
   mems: List[Node[T]],
   ctcs: List[T]) extends Group[T](n, mems, ctcs)

sealed abstract class FeatType
case object BoolFeat extends FeatType
case object TriFeat extends FeatType
case object IntFeat extends FeatType
case object StringFeat extends FeatType

