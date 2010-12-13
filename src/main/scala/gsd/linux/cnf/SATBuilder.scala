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

import org.sat4j.core.VecInt
import collection.mutable.ListBuffer
import gsd.linux.BExpr
import util.logging.Logged
import org.sat4j.specs.{IConstr, ISolver, ContradictionException}

trait SATSolver[T <: ISolver] {
  
  val solver: T = newSolver
  protected def newSolver: T

  def toVecInt(lits: Iterable[Int]): VecInt =
    new VecInt(lits.toArray)
}

trait DoneArray extends SATBuilder {

  /**
   * @return A vars x vars array where the 0th index is unused since
   * we ignore the 0th variable in the SAT solver.
   */
  def mkDoneArray(additional: Iterable[Int]) = {
    log("[DEBUG] Adding %d additional ignored variables".format(additional.size))

    val arr = Array.ofDim[Boolean](size + 1, size + 1)

    //Initialize self-tests to 'done'
    for (i <- 0 to size)
      arr(i)(i) = true

    //Initialize variable 0 to 'done'
    for (i <- 0 to size) {
      arr(0)(i) = true
      arr(i)(0) = true
    }

    //Initialize generated variables to 'done'
    for {
      g <- 0 to size if genArray(g)
      i <- 0 to size
    } {
      arr(g)(i) = true
      arr(i)(g) = true
    }

    for {
      i <- additional
      j <- 0 to size
    } {
      arr(i)(j) = true
      arr(j)(i) = true
    }
    arr
  }

}

/**
 * WARNING: The SAT solver has its own internal state, be careful about
 * calling certain stateful operations (i.e. like model) on the SAT solver.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
class SATBuilder(cnf: CNF, val size: Int, val genVars: Set[Int] = Set())
        extends SATSolver[ISolver] with Logged {

  def this(exprs: Iterable[BExpr], idMap: Map[String, Int], gens: Set[String]) =
    this(exprs flatMap { CNFBuilder.toCNF(_, idMap) }, idMap.size, gens map idMap.apply)

  log("[DEBUG] %d generated variables".format(genVars.size))

  val genArray: Array[Boolean] = {
    val result = Array.fill[Boolean](size + 1)(false)
    result(0) = true
    genVars foreach { result(_) = true }
    result
  }

  val realVars: Set[Int] =
    (1 to size).toSet -- (genArray.zipWithIndex filter { _._1 } map { _._2 })

  protected def newSolver: ISolver =
    SolverFactory.newDefault

  init

  private def init {
    log("[DEBUG] SAT solver has %d variables".format(size))
    solver newVar size

    //FIXME workaround for free variables not appearing in models
    for (i <- 1 to size)
      addClause(List(i, -i))

    addCNF(cnf)
  }

  def addCNF(cnf: CNF): Iterable[IConstr] =
    cnf flatMap addClause

  def addClause(clause: Clause): Option[IConstr] =
    try {
      assert(!clause.contains(0), "Clause cannot contain 0")
      val vi = toVecInt(clause)
      val res = solver.addClause(vi)
      vi.clear

      if (res != null) {
        constrToClause(res, clause)
        Some(res)
      }
      else None
    }
    catch {
      case e: ContradictionException => throw e
    }

  def removeConstr(c: IConstr) = {
    assert(c != null, "cannot remove a constraint that is null!")
    solver.removeConstr(c)
  }

  /**
   * Don't store constraint to clause mapping in base implementation
   */
  protected def constrToClause(constr: IConstr, clause: Clause): Unit = {}

  def isSatisfiable = solver.isSatisfiable

  def isSatisfiable(assump: Iterable[Int]) =
    solver.isSatisfiable(toVecInt(assump))

  def reset =
    solver.reset
  
  def reload {
    reset
    init
  }
}
