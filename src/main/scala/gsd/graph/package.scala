package gsd

import graph.Edge

package object graph {

  type EdgeMap[T] = Map[T,Set[T]]

  implicit def toTuple[T](e: Edge[T]) = e match {
    case Edge(source, target) => (source, target)
  }

  implicit def toRichEdgeMap[T](map : Map[T,Set[T]]) =
    new RichEdgeMap[T](map)

  class RichEdgeMap[T](map: Map[T,Set[T]]) {

    def +(t: Edge[T]): EdgeMap[T] = t match {
      case Edge(u,v) => map.get(u) match {
        case Some(vs) => map - u + Tuple2(u, vs + v)
        case None => map + Tuple2(u, Set(v))
      }
    }

    def -(t: Edge[T]): EdgeMap[T] = t match {
      case Edge(u,v) => map.get(u) match {
        case Some(vs) => map - u + Tuple2(u, vs - v)
        case None => map
      }
    }

    def --(ts : Iterable[Edge[T]]): EdgeMap[T] =
      ts.foldLeft(map)((x,y) => x - y)

    def ++(ts : Iterable[Edge[T]]): EdgeMap[T] =
      ts.foldLeft(map)((x,y) => x + y)

    /**
     * Strict conversion to RichEdgeMap
     */
    def toEdgeMap = this
    
  }

  def toMultiMap[T](ts : Iterable[Edge[T]]): EdgeMap[T] = {
    val edgeMap = new collection.mutable.HashMap[T,Set[T]]
    for (Edge(s,t) <- ts)
      edgeMap.put(s, edgeMap.getOrElse(s, Set()) + t)
    Map() ++ edgeMap
  }

}