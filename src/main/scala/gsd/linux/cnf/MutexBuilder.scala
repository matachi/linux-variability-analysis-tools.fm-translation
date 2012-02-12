package gsd.linux.cnf

import org.sat4j.minisat.orders.PositiveLiteralSelectionStrategy
import org.sat4j.minisat.SolverFactory
import gsd.graph.{Edge, UndirectedGraph}

trait MutexBuilder extends SATBuilder with DoneArray {

  override def newSolver = {
    val s = SolverFactory.newMiniLearningHeapRsatExpSimpBiere
    s.getOrder.setPhaseSelectionStrategy(new PositiveLiteralSelectionStrategy)
    s
  }

  //Initialization of the SAT solver performed in SATBuilder

  lazy val DISPROVER_ATTEMPTS: Int = size
  lazy val DISPROVER_LOTSIZE: Int  = size / 100

  val rand = new scala.util.Random

  /**
   * Returns true iff v1 excludes v2, false otherwise
   */
  def mutex(v1: Int, v2: Int) = !isSatisfiable(List(v1, v2))

  /**
   *  @param done A (vars + 1) x (vars + 1) array indicating whether the mutex
   *              has been tested.
   */
  def randomDisprover(done: Array[Array[Boolean]]): Array[Array[Boolean]] =
    randomDisprover(DISPROVER_LOTSIZE, DISPROVER_ATTEMPTS, done)

  /**
   * TODO Set a time-out.
   */
  def randomDisprover(lotSize: Int, attempts: Int, done: Array[Array[Boolean]]) = {
    def mkRandomizedLot: Seq[Int] =
      (0 until lotSize).map { _ => rand.nextInt(cutoffSize) + 1 }

    for (i <- 0 until attempts) {
      Console.print("MG: randomized disprover: %d / %d\r".format((i + 1), attempts))
      if (isSatisfiable(mkRandomizedLot))
        for {
          v1 <- 1 to cutoffSize if solver.model(v1)
          v2 <- 1 to cutoffSize if solver.model(v2)
        } {
          done(v1)(v2) = true
          done(v2)(v1) = true
        }
    }
    Console.println
    done
  }

  def mkMutexGraph[T <% Ordered[T]](
      varMap: Map[Int,T] = Map[Int,Int]() withDefault { v => v },
      additional: Iterable[Int] = Nil): UndirectedGraph[T] =
    mkMutexGraph(varMap, additional, randomDisprover(mkDoneArray(additional)))

  def mkMutexGraph[T <% Ordered[T]](varMap: Map[Int,T],
                      additional: Iterable[Int],
                      done: Array[Array[Boolean]]): UndirectedGraph[T] = {

    def markNonMutexes =
      for {
        i <- 1 to cutoffSize
        j <- 1 to cutoffSize if !done(i)(j) && solver.model(i) && solver.model(j)
      } {
        done(i)(j) = true
        done(j)(i) = true
      }

    val mutexes = new collection.mutable.ListBuffer[Edge[T]]

    for {
      i <- 1   to cutoffSize
      j <- i+1 to cutoffSize if !done(i)(j)
    } {
      Console.print("MG: %5d / %5d | %5d\r".format(i, size, j)) //Write on same line
      if (mutex(i,j)) {
        mutexes += Edge(varMap(i), varMap(j))
        done(i)(j) = true
        done(j)(i) = true
      }
      else markNonMutexes
    }
    Console.println
    new UndirectedGraph(((realVars -- additional) map varMap.apply).toSet, mutexes)
  }

}