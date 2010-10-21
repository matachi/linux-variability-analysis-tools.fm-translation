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

trait CNFRewriter extends Rewriter {

  val sDistributeRule: Strategy = innermost {
    rule {
      case BOr(BAnd(x,y),z) => BAnd(BOr(x,z), BOr(y,z))
      case BOr(x,BAnd(y,z)) => BAnd(BOr(x,y), BOr(x,z))
    }
  }

  def distribute(e: BExpr): List[BExpr] =
    rewrite(sDistributeRule)(e).splitConjunctions

}

trait CNFBuilder {

  val idMap: Map[String, Int]

  /**
   * Assumes expression has been simplified.
   *   - Nested negations don't exist.
   *   - been distributed
   */
//  def toCNF(in: BExpr): CNF = {
//    def t(e: BExpr): CNF = e match {
//      case BId(v) => List(idMap(v))
//      case BNot(BId(v)) => List(-idMap(v))
//      case BOr(x,y) => t(x) ::: t(y)
//      case BAnd(x,y) => -idMap(v) :: t(y)
//    }
//
//  }

}