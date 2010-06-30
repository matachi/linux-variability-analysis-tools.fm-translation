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

import FMUtil._
import TExprTranslation._
import AbstractSyntax.mkAChoice

object TFMTranslation {

  private type T = TExpr

  /**
   * The algorithm uses two maps for constructing the hierarchy:
   *     1. Hierarchy Map - A map of a feature to its immediate parent.
   *     2. Config Map - A map of a feature to its closest ancestor config.
   *
   * The SAT solver only contains configs since only configs can participate in
   * constraints. 
   */
  def mkFeatureModel(hi: Hierarchy, k: ConcreteKConfig): FM[T] = {

    def mkModel(ctcs: Map[String, List[TExpr]]): FM[T] = {

      //Roots are features that are not present in the parentMap
      val roots = k.features.filter { hi.contains }

      def mkFeature(c: CSymbol): List[Feature[T]] = c match {
        case c: CConfig => List(OFeature(c.id, mkType(c.ktype), ctcs(c.id),
                                           c.children flatMap { mkFeature }))
        case _: CMenu => List(MFeature(c.id, BoolFeat, Nil,
                                          c.children flatMap { mkFeature }))
        case _: CChoice => c.children flatMap { mkFeature }
      }

      def mkType(t: KType) = t match {
        case KBoolType => BoolFeat
        case KTriType => TriFeat
        case KIntType => IntFeat
        case KHexType => IntFeat //treat hex features as integers
        case KStringType => StringFeat
      }
      
      def mkGroup(c: AChoice): Group[T] =
         if (c.isBool) XorGroup(c.memIds, mkChoiceConstraints(c))
         else OrGroup(c.memIds, mkChoiceConstraints(c))
      
      FM(roots flatMap { mkFeature },
                   k.choices map { mkAChoice } map { mkGroup })
    }

    def mkConstraintMap(ak: AbstractKConfig): Map[String,List[TExpr]] = Map() ++
        (ak.configs map { c => c.id -> mkConfigConstraints (c) })

    mkModel(mkConstraintMap(k))
  }


}