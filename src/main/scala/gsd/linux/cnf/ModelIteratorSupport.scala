package gsd.linux.cnf

import org.sat4j.tools.ModelIterator
import org.sat4j.specs.{ContradictionException, ISolver}
import collection.mutable.ListBuffer

trait ModelIteratorSupport extends SATSolver[ISolver] {

  abstract override def newSolver: ModelIterator =
    new ModelIterator(super.newSolver)

  def allConfigurations(): List[Array[Int]] =
    allConfigurations(Nil)

  /**
   * TODO make this into a lazy Stream
   */
  def allConfigurations(assump: List[Int]): List[Array[Int]] = {
    val result = new ListBuffer[Array[Int]]
    try {
      while (solver.isSatisfiable(toVecInt(assump))) {
        val model = solver.model
        //        println(Arrays.toString(model))
        result += model
      }
    }
    catch {
      case e: ContradictionException => error(e.getMessage)//do nothing
      case e => error("what?" + e)
    }
    result.toList
  }


}