package gsd.linux

import org.scalacheck._
import Gen._

trait CNFGenerator {

  import cnf._

  case class GeneratedCNF(cnf: CNF, numVars: Int, numClauses: Int) {

    lazy val varMap: Map[Int, String] =
      ((1 to numVars) zip ((1 to numVars) map { _.toString })).toMap

    lazy val generated: Set[Int] =
      containerOf[Set, Int](choose(1, numVars)).sample match {
        case Some(x) => x
        case None => Set()
      }
  }

  /**
   * FIXME
   * Hack to convert 0's to 1's. If I use the suchThat generator, then an error
   * of too many discarded tests occur.
   */
  def genClause(numVars: Int): Gen[Clause] =
    for (clause <- listOf(choose(-numVars, numVars)))
      yield clause map { i => if (i == 0) 1 else i }

  def genCNF(numVars: Int, numClauses: Int): Gen[GeneratedCNF] =
   for (cnf <- containerOfN[List,Clause](numClauses, genClause(numVars)))
     yield GeneratedCNF(cnf, numVars, numClauses)

  implicit def arbGeneratedCNF: Arbitrary[GeneratedCNF] =
    Arbitrary(sized { s => genCNF(s + 1, s * 4) })
}