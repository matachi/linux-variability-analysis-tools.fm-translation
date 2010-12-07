package gsd.linux.cnf

import org.sat4j.tools.xplain.Xplain
import org.sat4j.minisat.SolverFactory
import collection.JavaConversions
import org.sat4j.specs.{IConstr, ISolver}
import collection.mutable.HashMap

trait XplainSupport extends SATSolver[Xplain[ISolver]] {
  this: ConstraintMap =>

  import JavaConversions._

  val xplain = new Xplain[ISolver](super.newSolver)
  
  abstract override def newSolver = xplain

  /**
   * Returns a list of clauses that causes an unsatisfiable result.
   * Can only be called after the solver returns !isSatisfiable.
   */
  def explain: List[Clause] =
    xplain.explain.toList map constraints.apply

}