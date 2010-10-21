package gsd.linux.cnf

import gsd.linux._
import kiama.rewriting.Rewriter

/**
 * A new CNF class, will replace old one.
 */
object CNF {

  type Clause = List[Int]
  type CNF = Iterable[Clause]

}

/**
 * FIXME temporarily here
 */
class IdMap extends collection.mutable.HashMap[String, Int] {
  def +=(e: BExpr) {
    e.identifiers filter { !contains(_) } foreach { id =>
      this += id -> this.size
    }
  }
}

object CNFBuilder extends Rewriter {

  import CNF._

  val sDistributeRule: Strategy = innermost {
    rule {
      case BOr(BAnd(x,y),z) => BAnd(BOr(x,z), BOr(y,z))
      case BOr(x,BAnd(y,z)) => BAnd(BOr(x,y), BOr(x,z))
    }
  }

  val sIffRule = everywheretd {
    rule {
      case BIff(x,y) => (!x | y) & (!y | x)
    }
  }

  val sImpliesRule = everywheretd {
    rule {
      case BImplies(x,y) => !x | y
    }
  }

  def distribute(e: BExpr): BExpr =
    rewrite(sDistributeRule)(e)

  def toClause(e: BExpr, idMap: collection.Map[String, Int]): Clause = e match {
    case BNot(BId(v)) => List(-idMap(v))
    case BId(v) => List(idMap(v))
    case BOr(x, y) => toClause(x, idMap) ::: toClause(y, idMap)
    case _ => error("Wrong format. Expression is not a clause!")
  }

  def toCNF(e: BExpr, idMap: collection.Map[String, Int]) =
    rewrite(sIffRule <* sImpliesRule)(e)
        .simplify
        .splitConjunctions
        .flatMap { distribute(_).splitConjunctions }
        .map { toClause(_, idMap) }

}

