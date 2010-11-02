package gsd.linux

import cnf.{ImplBuilder, SATBuilder}
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

import gsd.graph._
import GraphBuilder.{mkDirectedGraph => g}

class ImplGraphTest extends AssertionsForJUnit {

  def mkCNF(clauses: (Int,Int)*) =
    clauses map { c => List(c._1, c._2) }

  @Test def simple {
    val cnf = mkCNF((-1,2), (-2,1))
    val sat = new SATBuilder(cnf,2) with ImplBuilder
    assert(sat.implication(1,2))
    assert(sat.implication(2,1))
  }

  @Test def neg {
    val cnf = mkCNF((-1,2), (2,3))
    val sat = new SATBuilder(cnf,2) with ImplBuilder
    assert(sat.implication(1,2))
    assert(!sat.implication(2,3))
  }

  @Test def implGraphClique {
    val cnf = mkCNF((-1,2), (-2,3), (-3,1))
    val sat = new SATBuilder(cnf,3) with ImplBuilder
    expect(g(1->2,1->3,2->3,2->1,3->1,3->2))(sat.mkImplicationGraph())
  }


}