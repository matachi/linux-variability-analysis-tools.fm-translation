package gsd.graph

case class Edge[V](source: V, target: V)

abstract class Graph[V](val vertices: Set[V], val edges: EdgeMap[V]) {

  def this(vs: Set[V], es: Iterable[Edge[V]]) =
    this(vs, toMultiMap(es))

  assert (!(edges exists { case (x,y) => y contains x }), "selp-loops not allowed: " + edges)

    val revEdges: EdgeMap[V] = toMultiMap {
      edges flatMap {
        case (src,tars) => tars map { Edge(_, src) }
      }
    } withDefaultValue Set()

  type This <: Graph[V]
  def New(newVs : Set[V], newEs: EdgeMap[V]) : This

  def +(t : Edge[V]): This = New(vertices, edges.toEdgeMap + t)
  def -(t : Edge[V]): This = New(vertices, edges.toEdgeMap - t)

  def ++(ts : Iterable[Edge[V]]): This = New(vertices, edges.toEdgeMap ++ ts)
  def --(ts : Iterable[Edge[V]]): This = New(vertices, edges.toEdgeMap -- ts)
}

case class DirectedGraph[V](vs: Set[V], es: EdgeMap[V]) extends Graph[V](vs,es) {

  type This = DirectedGraph[V]

  def New(newVs: Set[V], newEs: EdgeMap[V]) =
    DirectedGraph(newVs,newEs)

  def this(vs: Set[V], es: Iterable[Edge[V]]) =
    this(vs, toMultiMap(es))

  def reverseEdges = New(vs, revEdges)

}
