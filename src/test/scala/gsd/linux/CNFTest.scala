package gsd.linux

import org.junit.Test
import org.scalatest.Suite
import org.scalatest.prop.Checkers

import org.scalacheck.Prop._

import CNF._
import IdMapBuilder._
import BDDBuilder._

class CNFTest extends Suite with Checkers with ExpressionGenerator {

  implicit def toBId(s: String) = B2Id(s)

  /**
   * Tests that if e is satisfiable, then the Tseitin transformation should
   * also be satisfiable with the same assignment.
   */
  @Test def testCNFUsingBDDs = check {
    forAll { (e: B2Expr) =>
      println("Testing: " + e)
      val cnf = e.toCNF.toExpression
      val bb = new BDDBuilder(mkIdMap(identifiers(cnf)))
      val result = bb.mkBDD(cnf).andWith(bb.mkBDD(e).satOne)
      begin0(result != bb.zero)(result.free)
    }
  }
  
}