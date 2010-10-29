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
package gsd.linux.cnf

import org.sat4j.minisat.SolverFactory

import CNF._
import org.sat4j.core.VecInt
import org.sat4j.specs.ContradictionException
import org.sat4j.tools.ModelIterator
import collection.mutable.ListBuffer
import gsd.linux.BExpr
import org.sat4j.reader.InstanceReader
import java.util.Arrays
import util.logging.Logged

/**
 * WARNING: The SAT solver has its own internal state, be careful about
 * calling certain stateful operations (i.e. like model) on the SAT solver.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
class SATBuilder(cnf: CNF, size: Int) extends Logged {

  def this(exprs: Iterable[BExpr], idMap: Map[String, Int]) =
    this(exprs flatMap { CNFBuilder.toCNF(_, idMap) }, idMap.size)
  
  val solver = newSolver
  protected def newSolver = new ModelIterator(SolverFactory.newDefault)
  addCNF(cnf, size)

  def addCNF(cnf: CNF, size: Int) = {
    log("# of Vars: " + size)
    solver.newVar(size)

    //FIXME workaround for free variables not appearing in models
    for (i <- 1 to size)
      addClause(List(i, -i))
    
    cnf foreach addClause
  }

  def addClause(clause: Clause): Unit =
    try {
      assert(!clause.contains(0), "Clause cannot contain 0")
      val vi = toVecInt(clause)
      solver.addClause(vi)
      vi.clear
    }
    catch {
      case e: ContradictionException => throw e
    }

  def allConfigurations(): List[Array[Int]] =
    allConfigurations(Nil)

  /**
   * TODO make this into a lazy Stream
   */
  def allConfigurations(assump: List[Int]): List[Array[Int]] = {
    val result = new ListBuffer[Array[Int]]
    try {
      while (solver.isSatisfiable(toVecInt(assump))) {
        val model = solver.model
//        println(Arrays.toString(model))
        result += model
      }
    }
    catch {
      case e: ContradictionException => error(e.getMessage)//do nothing
      case e => error("what?" + e)
    }
    result.toList
  }

  def isSatisfiable = solver.isSatisfiable
  def isSatisfiable(assump: List[Int]) =
    solver.isSatisfiable(toVecInt(assump))

  def reset = solver.reset
  
  def reload {
    reset
    addCNF(cnf, size)
  }

  def toVecInt(lits: List[Int]): VecInt =
    new VecInt(lits.toArray)
}
