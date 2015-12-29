package gsd.linux.cnf

import org.sat4j.core.VecInt

import gsd.graph._


/**
 * Extension to the SATBuilder enabling the construction of an implication graph.
 */
trait ImplBuilder extends SATBuilder with DoneArray {

  /**
   * Same effect as isSatisfiable, but with a different name to avoid problems
   * with type erasure.
   */
  def isVarsSat(vs: Int*) = solver.isSatisfiable(new VecInt(vs.toArray))
  
  /**
   * Returns true iff v1 implies v2, false otherwise
   */
  def implication(v1: Int, v2: Int): Boolean = !isSatisfiable(List(v1, -v2))

  /**
   * Dead features should be removed prior to calling this otherwise these
   * dead features will have implications to all items features!
   *
   * Optimization taken from Nele's implementation: If the formula is
   * satisfiable after a check to implication, then we examine the resulting
   * model. In that model, if there exists i = TRUE, and j = FALSE, then we
   * know that i does NOT imply j. Look at the truth table for implication.
   *
   * @param varMap mapping from variable to its identifier
   * @param additional any additional variables to ignore
   */
  def mkImplicationGraph[T](
          varMap: Map[Int,T] = Map[Int,Int]() withDefault { v => v },
          additional: Iterable[Int] = Nil)
      : DirectedGraph[T] = {

    logger.info("[DEBUG] Adding %d additional ignored variables".format(additional.size))
    logger.info("[DEBUG] %d remain in the resulting implication graph".format(cutoffSize - additional.size))

    val done = mkDoneArray(additional)

    // For debugging purposes
    def countRemaining = {
      var count = 0

      for {
        i <- 1 to cutoffSize
        j <- 1 to cutoffSize if !done(i)(j)
      } count += 1

      count
    }

    val numTotal = countRemaining
    val result = new collection.mutable.ListBuffer[Edge[T]]

    for (i <- 1 to cutoffSize) {
      val startTime = System.currentTimeMillis()
      var numDone = 0

      for (j <- 1 to cutoffSize if !done(i)(j)) {
        if (implication(i,j)) {
          result += Edge(varMap(i), varMap(j))

          done(i)(j) = true
          numDone += 1
        }
        else {
          // mark non-implications using the computed model
          for {
            i <- 1 to cutoffSize
            j <- 1 to cutoffSize if solver.model(i) && !solver.model(j) && !done(i)(j)
          } {
            done(i)(j) = true
            numDone += 1
          }
        }
      }

      Console.print(
        "IG: %5d / %5d (time: %3d s) (done: %d / %d, %d remaining)\r".format(
          i,
          cutoffSize,
          (System.currentTimeMillis() - startTime) / 1000,
          numDone,
          numTotal,
          countRemaining // FIXME inefficient
        )) //Write on same line
    }
    Console.println("Done!")

    new DirectedGraph[T](((realVars -- additional) map varMap.apply).toSet, result)
  }
}