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

object FMUtil {

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

    println("Proper Parents: ")
    properParentsPrime map { case(x,y) => x.id -> y.id } foreach println 


    //Return the new hierarchy.
    //Features that don't have a proper parent belong at the root (they don't
    //exist in the map).
    Map() ++ configMap ++ properParentsPrime -- rooted
  }

}