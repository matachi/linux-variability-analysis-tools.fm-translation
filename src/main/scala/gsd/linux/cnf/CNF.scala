package gsd.linux.cnf

import gsd.linux._
import org.kiama.rewriting.Rewriter._
import org.kiama.rewriting.Strategy
import collection.mutable.HashMap

object IdMap {
  def apply(es: Iterable[BExpr]): Map[String, Int] = {
    val map = new HashMap[String, Int]
    es foreach { e =>
      e.identifiers filter { !map.contains(_) } foreach { id =>
        map += id -> (map.size + 1)
      }
    }
    Map() ++ map
  }
}


object CNFBuilder {

  val sDistributeRule: Strategy = oncetd {
    rule[BExpr] {
      case BOr(BAnd(x,y),z) => BAnd(BOr(x,z), BOr(y,z))
      case BOr(x,BAnd(y,z)) => BAnd(BOr(x,y), BOr(x,z))
    }
  }

  val sIffRule = everywheretd {
    rule[BExpr] {
      case BIff(x,y) => (!x | y) & (!y | x)
    }
  }

  val sImpliesRule = everywheretd {
    rule[BExpr] {
      case BImplies(x,y) => !x | y
    }
  }

  /**
   * Run until we reach a fixpoint.
   */
  def distribute(e: BExpr): List[BExpr] = {
    val result = rewrite(sDistributeRule)(e)
    if (result == e) result.splitConjunctions
    else result.splitConjunctions flatMap distribute
  }

  /**
   * @param idMap Maps identifiers in the expression to an integer
   */
  def toClause(e: BExpr, idMap: collection.Map[String, Int]): Clause = e match {
    case BNot(BId(v)) => List(-idMap(v))
    case BId(v) => List(idMap(v))
    case BOr(x, y) => toClause(x, idMap) ::: toClause(y, idMap)
    case _ => sys.error("Wrong format. Expression is not a clause: " + e)
  }

  /**
   * @param idMap Maps identifiers in the expression to an integer
   */
  def toCNF(e: BExpr, idMap: collection.Map[String, Int]) =
    rewrite(sIffRule <* sImpliesRule)(e)
        .simplify
        .splitConjunctions
        .filter { _ != BTrue }
        .flatMap { distribute }
        .map { toClause(_, idMap) }

}

