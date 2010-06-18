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

object FMTranslation2 extends BooleanTranslation {

  type ParentIdMap = Map[String, Option[String]]

  /**
   * Returns a list of the ancestors of child beginning with its immediate parent.
   */
  def mkAncestorList(parentMap: ParentIdMap)(child: String): List[String] =
    parentMap(child) match {
      case Some(parent) => parent :: mkAncestorList (parentMap)(parent)
      case None => Nil
    }

  def mkFeatureModel(sat: SATBuilder, parentMap: ParentIdMap) = {

    //Remove features at the root and dead features
    val process = parentMap.toList.remove {
      case (child, None) => true
      case (child,_) => !sat.isSatisfiable(PosLit(child))
    }

    //Features that violate the hierarchy
    val violating = process.filter {
      case (child, Some(parent)) => !sat.isSatisfiable(PosLit(child), PosLit(parent))
      case _ => error("Should be unreachable.")
    }

    val properParents = violating.map { case (child,_) =>
      //Ignore first parent since we know it is not satisfiable.
      //We previously filtered out rooted features so we have > 0 ancestors.
      val ancestors = mkAncestorList(parentMap)(child).tail
      val pParent = ancestors.find { p =>
        sat.isSatisfiable(PosLit(child), PosLit(p))
      }
      child -> pParent
    }

    properParents.foreach { println }
  }


}