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

object FMTranslation {

  type Hierarchy = Map[CSymbol, CSymbol]

  /**
   * @param pm A map from configs to their parent (could be a config, menu or choice).
   * @return a list of the ancestors of child beginning with its immediate parent.
   */
  def mkAncestorList(pm: Map[CSymbol, CSymbol])(child: CSymbol): List[CSymbol] =
    pm.get(child) match {
      case Some(parent) => parent :: mkAncestorList (pm)(parent)
      case None => Nil
    }


  def mkProperParents(sat: SATBuilder, k: ConcreteKConfig): Hierarchy = {

    //A map from all features to their closest ancestor that's a Config
    val configMap = Hierarchy.mkParentMap(k)

    //Remove menus, choices and dead features from the parentMap
    val process = configMap.toList remove {
      case (_:CMenu,_) | (_:CChoice,_) => true
      case (child,_) => !sat.isSatisfiable(PosLit(child.id))
    }

    //Filter features that violate the hierarchy -- they do not imply their
    //parents
    val violating = process filter {
      case (child, par) => !sat.isSatisfiable(PosLit(child.id), PosLit(par.id))
    }

    val properParents = violating map { case (child,_) =>

      //Ignore first parent since we know it is not satisfiable.
      //We previously filtered out rooted features so we have > 0 ancestors.
      val ancestors = mkAncestorList(configMap)(child).tail

      //FIXME not nesting under the closest menu or choice
      val pParent: Option[CSymbol] = ancestors.find {
        case p: CConfig => sat.isSatisfiable(PosLit(child.id), PosLit(p.id))
        case _ => false
      }

      //Note: a proper parent may not be found
      child -> pParent
    }

    val (rooted, properParentsPrime) = properParents partition {
      case (_,None) => true
      case _ => false
    } match {
      case (r,pp) =>
        //Rooted features just retain the key, proper parents remove Option
        (r map { case (k,_) => k }) -> (pp map { case (k,v) => k -> v.get })
    }


    //Return the new hierarchy.
    //Features that don't have a proper parent belong at the root (they don't
    //exist in the map).
    Map() ++ configMap ++ properParentsPrime -- rooted
  }

  /**
   * The algorithm uses two maps for constructing the hierarchy:
   *     1. Hierarchy Map - A map of a feature to its immediate parent.
   *     2. Config Map - A map of a feature to its closest ancestor config.
   *
   * The SAT solver only contains configs since only configs can participate in
   * constraints. 
   */
  def mkFeatureModel(hi: Hierarchy, k: ConcreteKConfig) = {

    def mkModel(ctcs: Map[String, List[TExpr]]) = {

      //Roots are features that are not present in the parentMap
      val roots = k.features.filter { hi.contains }

      def mkFeature(c: CSymbol): List[Feature] = c match {
        case c: CConfig => List(OptFeature(c.id, mkType(c.ktype), ctcs(c.id),
                                           c.children flatMap { mkFeature }))
        case _: CMenu => List(MandFeature(c.id, BoolFeat, Nil,
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
      
      def mkGroup(c: CChoice): XorGroup = XorGroup(c.children map { _.id })
      
      FeatureModel(roots flatMap { mkFeature }, k.choices map { mkGroup })
    }

    import TExprTranslation._
    def mkConstraintMap(ak: AbstractKConfig): Map[String,List[TExpr]] = Map() ++
        (ak.configs map { c => c.id -> (mkBounds(c) :: mkDefaults(c)) })

    mkModel(mkConstraintMap(k))
  }


}