package gsd.graph

import gsd.graph._
import GraphBuilder.{mkDirectedGraph => g}
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

class GraphTest extends AssertionsForJUnit {

  @Test def graphBuilder {
    expect(DirectedGraph(Set(), Map()))(g(Nil))
    expect(DirectedGraph(Set(1,2,3), Map()))(g(List(1,2,3)))
    expect(new DirectedGraph(Set(1,2,3),
      List(1->2, 2->3, 3->1) map { Function.tupled(Edge.apply) }))(g(1->2,2->3,3->1))
  }

  @Test def addEdge {
    expect(g(1->2,2->3,3->1))(g(1->2, 2->3) + (3 -> 1))
    expect(g(1->2))(g(List(1,2)) + (1->2))
    expect(g(1->2,2->1))(g(List(1,2)) + (1->2) + (2->1))
    expect(g(1->2,2->1))(g(List(1,2)) ++ List(1->2, 2->1))
  }

  @Test def removeEdge {
    expect(g(List(1,2,3), 1->2))(g(1->2, 2->3) - (2->3))
    expect(g(List(1,2)))(g(List(1,2)) - (1->2))
  }

  @Test def toParseString {
    expect("")(g().toParseString)
    expect("1: 1;\n2: 2;\n")(g(Set(1,2)).toParseString)
    expect("1: 1;\n2: 2;\n3: 3;\n1->2;2->3;3->1;")(g(1->2, 2->3, 3->1).toParseString)
  }
}