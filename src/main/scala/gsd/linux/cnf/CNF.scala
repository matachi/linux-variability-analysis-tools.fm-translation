package gsd.linux.cnf

import gsd.linux._
import kiama.rewriting.Rewriter
import collection.mutable.HashMap
import util.logging.Logged
import actors.{TIMEOUT, Actor}

/**
 * A new CNF class, will replace old one.
 */
object CNF {

  type Clause = List[Int]
  type CNF = Iterable[Clause]

}

object IdMap {
  def apply(es: Iterable[BExpr]): Map[String, Int] = {
    val map = new HashMap[String, Int]
    es foreach { e =>
      e.identifiers filter { !map.contains(_) } foreach { id =>
        map += id -> map.size
      }
    }
    Map() ++ map
  }
}


object CNFBuilder extends Rewriter {

  import CNF._
  import Actor._


  /**
   *
   * TODO change to class
   */
  object Distribute {

    /**
     * TODO factor this out so it's not stateful
     */
    object IdGen {
      val prefix = "_G"
      var i = 0
      def next = { i+=1; prefix + i }
      def allIds = (1 to i).map { prefix + _ }.toList
    }

    /**
     * Substitute an expression with an identifier n. One pass of a Tseitin
     * transform. Runs distribute on the substituted expression.
     */
    def subst(e: BExpr, timeout: Long): List[BExpr] = {
      val n = IdGen.next
      var ret: BExpr = null
      val subbed = rewrite{
        oncetd {
          rule {
            case BOr(x@BAnd(_,_),rest) =>
              ret = x
              BOr(BId(n),rest)
            case BOr(rest,x@BAnd(_,_)) =>
              ret = x
              BOr(BId(n),rest)
          }
        }
      }(e)

      // Add equivalances and distribute on the new substituted expression
      if (ret != null) {
        println("Substituting " + n + " for " + ret)
        (distribute(ret, timeout) map
                { BId(n) iff _ } flatMap
                { _.splitConjunctions }) ::: distribute(subbed, timeout)
      }
      else if (subbed == e) e.splitConjunctions //FIXME messy
      else distribute(e, timeout * 2)
    }

    val sDistributeRule: Strategy = innermost {
      rule {
        case BOr(BAnd(x,y),z) => BAnd(BOr(x,z), BOr(y,z))
        case BOr(x,BAnd(y,z)) => BAnd(BOr(x,y), BOr(x,z))
      }
    }

    val dist: Actor = actor {
      Actor.loop {
        react {
          case e: BExpr =>
            reply(rewrite(sDistributeRule)(e))
        }
      }
    }

    def distribute(e: BExpr, timeout: Long): List[BExpr] = {
      println("Distributing (timeout=" + timeout + "): " + e)
      val res = dist !? (timeout, e)
    
      res match {
        case Some(ret: BExpr) => ret.splitConjunctions
        case None => subst(e, timeout)
        case x => error("Undefined return value: " + x)
      }
    }

    def distribute(exprs: List[BExpr]): List[BExpr] =
      exprs flatMap { distribute(_, 5000L) }
    
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

  /**
   * @param idMap Maps identifiers in the expression to an integer
   */
  def toClause(e: BExpr, idMap: collection.Map[String, Int]): Clause = e match {
    case BNot(BId(v)) => List(-idMap(v))
    case BId(v) => List(idMap(v))
    case BOr(x, y) => toClause(x, idMap) ::: toClause(y, idMap)
    case _ => error("Wrong format. Expression is not a clause: " + e)
  }

  /**
   * @param idMap Maps identifiers in the expression to an integer
   */
  def toCNF(e: BExpr, idMap: collection.Map[String, Int]): CNF = {
    val exprs =
      rewrite(sIffRule <* sImpliesRule)(e)
        .simplify
        .splitConjunctions

    val dist = Distribute.distribute(exprs)

    Nil
//        .flatMap { distribute(_).splitConjunctions }
//        .map { toClause(_, idMap) }
  }

}

