package gsd.linux

import cnf.{ImplBuilderStats, ImplBuilder, SATBuilder}
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

import gsd.graph._
import GraphBuilder.{mkDirectedGraph => g}

class ImplGraphTest extends AssertionsForJUnit {

  implicit def toList[T](t: (T,T)): List[T] =
    List(t._1, t._2)

  def mkCNF(clauses: List[Int]*) =
    clauses.toList

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
    val sat = new SATBuilder(cnf,3) with ImplBuilder with ImplBuilderStats
    expect(g(1->2,1->3,2->3,2->1,3->1,3->2))(sat.mkImplicationGraph())
    expect(3)(sat.numImpl)
    expect(3)(sat.numImplCalls)
  }

  @Test def genVars1 {
    val cnf = mkCNF((-1,2), (-2,3), (-3,1), List(3,4,5))
    val sat = new SATBuilder(cnf,5, Set(4,5)) with ImplBuilder with ImplBuilderStats
    expect(g(1->2,1->3,2->3,2->1,3->1,3->2))(sat.mkImplicationGraph())
  }

  @Test def genVars2 {
    val cnf = mkCNF((-1,2), (-2,3), (-3,4))
    val sat = new SATBuilder(cnf,4,Set(4)) with ImplBuilder with ImplBuilderStats
    println(sat.mkImplicationGraph())
  }


}