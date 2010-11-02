package gsd.graph

object GraphBuilder {

  def mkDirectedGraph[T](): DirectedGraph[T] =
    DirectedGraph(Set(), Map())

  def mkDirectedGraph[T](vertices: Iterable[T]): DirectedGraph[T] =
    DirectedGraph(Set() ++ vertices, Map())

  def mkDirectedGraph[T](edges: (T,T)*): DirectedGraph[T] =
    new DirectedGraph (
      Set() ++ edges flatMap { case (x,y) => List(x,y) },
      edges map (Function.tupled(Edge.apply))
    )

  def mkDirectedGraph[T](vertices: Iterable[T], edges: (T,T)*): DirectedGraph[T] =
    new DirectedGraph ( Set() ++ vertices, edges map (Function.tupled(Edge.apply)) )

}