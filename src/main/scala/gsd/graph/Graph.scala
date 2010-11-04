package gsd.graph

case class Edge[V](source: V, target: V)

abstract class Graph[V](val vertices: Set[V], val edges: EdgeMap[V])
        extends GraphWriter[V] {

  def this(vs: Set[V], es: Iterable[Edge[V]]) =
    this(vs, toMultiMap(es))

  assert (!(edges exists { case (x,y) => y contains x }),
    "selp-loops not allowed: " + edges)
  
  assert ((edges forall {
    case (x,y) => (vertices contains x) && (y forall (vertices contains))
  }), "Edge contains vertex that is not in this graph!")

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

  def toParseString(implicit toOrdered: V => Ordered[V]): String
}

case class DirectedGraph[V](vs: Set[V], es: EdgeMap[V]) extends Graph[V](vs,es) {

  type This = DirectedGraph[V]

  def New(newVs: Set[V], newEs: EdgeMap[V]) =
    DirectedGraph(newVs,newEs)

  def this(vs: Set[V], es: Iterable[Edge[V]]) =
    this(vs, toMultiMap(es))

  def reverseEdges = New(vs, revEdges)

  def toParseString(implicit toOrdered: V => Ordered[V]) =
    mkParseString("->")

}

trait GraphWriter[V] {
  this: Graph[V] =>

  def mkParseString(edgeSep: String)
                      (implicit toOrdered: V => Ordered[V]): String = {

    val sb = new StringBuilder

    val vmap = Map() ++ {
      vertices.toList
          .sortWith { (x,y) => toOrdered(x) < y }
          .zipWithIndex
          .map { case (v,i) => (v, i+1) }
    }

    for ((v, id) <- vmap)
      sb append id append ": " append v append ";\n"

    var len = 0
    for {
      (src, targets) <- edges.toList sortWith { case ((x,_),(y,_)) => x < y }
      tar <- targets.toList sortWith { _ < _ }
    } {
      val prev = sb.length
      sb append vmap(src) append edgeSep append vmap(tar) append ";"
      val curr = sb.length

      len += curr - prev

      if (len > 80) {
        len = 0
        sb append "\n"
      }
    }
    
    sb toString
  }
  
}


