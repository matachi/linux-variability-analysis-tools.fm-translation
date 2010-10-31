package gsd.linux

import cnf.CNFBuilder
import org.junit.Test
import org.scalatest.Suite
import org.scalatest.prop.Checkers

import org.scalacheck.Prop._

import CNF._
import IdMapBuilder._
import BDDBuilder._

class CNFTest extends Suite with Checkers with ExpressionGenerator {

  implicit def toBId(s: String) = BId(s)

  val idMap: Map[String, Int] = Map() ++ {
    (('a' to 'z') map { "" + _ }).toList.zipWithIndex map { case (c,i) => (c, i + 1) }
  }

  @Test def nesting {
    val e = ("a" | "b") | "c" | "d"
    expect(List(List(1,2,3,4)))(CNFBuilder.toCNF(e, idMap))
  }




  /**
   * Tests that if e is satisfiable, then the Tseitin transformation should
   * also be satisfiable with the same assignment.
   */
//  @Test def testCNFUsingBDDs = check {
//    forAll { (e: BExpr) =>
//      println("Testing: " + e)
//      val cnf = e.toCNF.toExpression
//      val bb = new BDDBuilder(mkIdMap(identifiers(cnf)))
//      val result = bb.mkBDD(cnf).andWith(bb.mkBDD(e).satOne)
//      begin0(result != bb.zero)(result.free)
//    }
//  }
  
}