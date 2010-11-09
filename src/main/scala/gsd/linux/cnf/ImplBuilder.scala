package gsd.linux.cnf

import org.sat4j.core.VecInt

import gsd.graph._


/**
 * Extension to the SATBuilder enabling the construction of an implication graph.
 */
trait ImplBuilder extends SATBuilder {

  /**
   * Same effect as isSatisfiable, but with a different name to avoid problems
   * with type erasure.
   */
  def isVarsSat(vs: Int*) = solver.isSatisfiable(new VecInt(vs.toArray))
  
  /**
   * @return A vars x vars array where the 0th index is unused since
   * we ignore the 0th variable in the SAT solver.
   */
  def mkDoneArray = {
    val arr = Array.ofDim[Boolean](size + 1, size + 1)

    //Initialize self-tests to 'done'
    for (i <- 0 to size)
      arr(i)(i) = true

    //Initialize variable 0 to 'done'
    for (i <- 0 to size) {
      arr(0)(i) = true
      arr(i)(0) = true
    }

    //Initialize generated variables to 'done'
    for {
      g <- 0 to size if genArray(g)
      i <- 0 to size
    } {
      arr(g)(i) = true
      arr(i)(g) = true
    }
    arr
  }

  /**
   * Returns true iff v1 implies v2, false otherwise
   */
  def implication(v1: Int, v2: Int): Boolean = !isSatisfiable(List(v1, -v2))

  /**
   * Dead features should be removed prior to calling this otherwise these
   * dead features will have implications to all other features!
   *
   * Optimization taken from Nele's implementation: If the formula is
   * satisfiable after a check to implication, then we examine the resulting
   * model. In that model, if there exists i = TRUE, and j = FALSE, then we
   * know that i does NOT imply j. Look at the truth table for implication.
   */
  def mkImplicationGraph[T](
          varMap: Map[Int,T] = Map[Int,Int]() withDefault { v => v },
          done: Array[Array[Boolean]] = mkDoneArray)
      : DirectedGraph[T] = {

    def markNonImplications =
      for {
        i <- 1 to size
        j <- 1 to size if !done(i)(j) && solver.model(i) && !solver.model(j)
      } {
        done(i)(j) = true
      }

    val result = new collection.mutable.ListBuffer[Edge[T]]

    for {
      i <- 1 to size
      j <- 1 to size if !done(i)(j)
    } {
      Console.print("IG: %5d / %5d | %5d\r".format(i, size, j)) //Write on same line
      if (implication(i,j)) {
        result += Edge(varMap(i), varMap(j))
        done(i)(j) = true
      }
      else markNonImplications
    }
    Console.println("Done!")

    new DirectedGraph(Set() ++ (realVars map varMap.apply), result)

  }


}