package gsd.linux.cnf

import collection.mutable.HashMap
import org.sat4j.specs.IConstr

trait ConstraintMap extends SATBuilder {
  
  val constraints = new HashMap[IConstr, Clause]

  override def constrToClause(constr: IConstr, clause: Clause) =
    constraints += constr -> clause

}