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


import org.sat4j.minisat.SolverFactory

import CNF._
import org.sat4j.core.VecInt
import org.sat4j.specs.ContradictionException

import IdMapBuilder._

/**
 * WARNING: The SAT solver has its own internal state, be careful about
 * calling certain stateful operations (i.e. like model) on the SAT solver.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
class SATBuilderOld(val idMap: Map[String,Int]) {

  lazy val varMap = idMap.inverse
  val solver = newSolver
  protected def newSolver = SolverFactory.newDefault

  def addCNF(cnf: CNF) = {
    solver.newVar(idMap.size)
    cnf.foreach { addClause }
  }

  def addClause(clause: Clause) =
    try {
      solver.addClause(toVecInt(clause))
    }
    catch {
      case e: ContradictionException => throw e
    }

  def isSatisfiable = solver.isSatisfiable
  def isSatisfiable(clause: Clause) = solver.isSatisfiable(toVecInt(clause))
  def isSatisfiable(lits: Lit*) : Boolean = isSatisfiable(lits.toList)

  def reset = solver.reset

  def toInt(lit: Lit) = lit match {
    case PosLit(x) => idMap(x)
    case NegLit(x) => -idMap(x)
  }

  def toIntArray(clause: Clause) = clause.map(toInt).toArray
  def toVecInt(clause: Clause) =  new VecInt(toIntArray(clause))
  def toLiterals(arr: Array[Int]) = arr.map { i =>
    if (i < 0) NegLit(varMap(-i)) else PosLit(varMap(i))
  }.toList

}
