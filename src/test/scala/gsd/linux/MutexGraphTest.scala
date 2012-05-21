package gsd.linux

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

import cnf._


class MutexGraphTest extends AssertionsForJUnit {

  implicit def toList[T](t: (T,T)): List[T] =
    List(t._1, t._2)

  def mkCNF(clauses: List[Int]*) =
    clauses.toList

  def mkBuilder(cnf: CNF, size: Int) =
    new SATBuilder(cnf, size) with MutexBuilder

  @Test def simple1 {
    val b = mkBuilder(mkCNF((-1,-2)), 3)
    assert(b.mutex(1,2))
    assert(!b.mutex(1,3))
    assert(!b.mutex(2,3))
  }

  @Test def simple2 {
    val b = mkBuilder(mkCNF((-1,-2), (-1,-3)), 3)
    assert(b.mutex(1,2))
    assert(b.mutex(1,3))
    assert(!b.mutex(2,3))
  }

  @Test def simple3 {
    val b = mkBuilder(mkCNF((-1,-2), (-1,-3), (-2, -3)), 3)
    assert(b.mutex(1,2))
    assert(b.mutex(2,1))
    assert(b.mutex(1,3))
    assert(b.mutex(3,1))
    assert(b.mutex(2,3))
    assert(b.mutex(3,2))
  }

}
