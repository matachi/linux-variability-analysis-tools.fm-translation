package gsd.linux

import cnf.CNFBuilder

package object cnf {

  type Clause = List[Int]
  type CNF = Iterable[Clause]

  implicit def toRichCNF(in: CNF) = new RichCNF(in)

  class RichCNF(in: CNF) {

    def toDimacs(varMap: Map[Int, String],
                 gens: Set[Int] = Set()): String = {
      val sb = new StringBuilder

      for ((v,id) <- varMap.toList sortWith { case ((v1,_), (v2,_)) => v1 < v2 })
        if (gens contains v) sb append "c %d$ %s\n".format(v, id)
        else sb append "c %d %s\n".format(v, id)

      //number of variables, number of clauses
      sb append "p cnf %d %d\n".format(varMap.size, in.toList.size)

      for (clause <- in) sb append clause.mkString(" ") append "\n"

      sb.toString
    }

  }

  implicit def toRichBExprList(in: List[BExpr]) = new {
    def toCNF(idMap: Map[String, Int]): CNF =
      in flatMap { CNFBuilder.toCNF(_, idMap) }
  }

}