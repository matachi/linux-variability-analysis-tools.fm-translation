package gsd.graph

object GraphBuilder {

  def mkDirectedGraph[T](): DirectedGraph[T] =
    new DirectedGraph(Set[T](), Nil)

  def mkDirectedGraph[T](vertices: Iterable[T]): DirectedGraph[T] =
    new DirectedGraph(Set() ++ vertices, Nil)

  def mkDirectedGraph[T](edges: (T,T)*): DirectedGraph[T] =
    new DirectedGraph (
      Set() ++ edges flatMap { case (x,y) => List(x,y) },
      edges map (Function.tupled(Edge.apply))
    )

  def mkDirectedGraph[T](vertices: Iterable[T], edges: (T,T)*): DirectedGraph[T] =
    new DirectedGraph(vertices.toSet, edges map (Function.tupled(Edge.apply)) )


  def mkUndirectedGraph[T <% Ordered[T]](vs: Iterable[T], edges: (T,T)*): UndirectedGraph[T] =
    new UndirectedGraph (vs.toSet, edges map Function.tupled(Edge.apply))

}