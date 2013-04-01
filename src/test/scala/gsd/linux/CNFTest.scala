package gsd.linux

import cnf.DimacsReader.{DimacsProblem, DimacsHeader}
import org.scalatest.Suite
import org.scalatest.prop.Checkers

import org.scalacheck.Prop._

import cnf._
import BDDBuilder._
import org.junit.{Ignore, Test}

class CNFTest extends Suite with Checkers with ExpressionGenerator with CNFGenerator {

  implicit def toBId(s: String) = BId(s)

  val idMap: Map[String, Int] = Map() ++ {
    (('a' to 'z') map { "" + _ }).toList.zipWithIndex map { case (c,i) => (c, i + 1) }
  }
  val varMap: Map[Int, String] = Map() ++ (idMap map { case (id,v) => (v, id) })

  def mkCNF(clauses: List[Int]*) =
    clauses.toList

  @Test def nesting {
    val e = ("a" | "b") | "c" | "d"
    expect(List(List(1,2,3,4)))(CNFBuilder.toCNF(e, idMap))
  }

//  @Test def dimacsWriter {
//    val cnf = mkCNF(List(1,2), List(-1,-2))
//    expect("c 1 a\nc 2 b\nc 3 c\np cnf 3 2\n1 2 0\n-1 -2 0\n")(cnf.toDimacs(Map(1->"a", 2->"b",3->"c")))
//    expect("c 1 a\nc 2 b\nc 3$ c\np cnf 3 2\n1 2 0\n-1 -2 0\n")(cnf.toDimacs(Map(1->"a", 2->"b",3->"c"), Set(3)))
//  }

  @Test def dimacsReader {
    val in = "c 1 a\nc 2 b\nc 3$ c\nc 4$ d\np cnf 4 2\n1 2 0\n-1 -2 0\n"
    val header = DimacsReader.readHeaderString(in)
    expect(DimacsHeader(Map(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d"), Map(), Set(3, 4), 3))(header)
    val problem = DimacsReader.readString(in)
    expect(DimacsProblem(4, List(List(1, 2), List(-1, -2))))(problem)
  }

  @Test def dimacsAuto = check {
    forAll { (c: GeneratedCNF) =>
      DimacsReader.readString(c.cnf.toDimacs(c.varMap, c.generated)).cnf == c.cnf
    }
  }

  /**
   * Tests that if e is satisfiable, then the Tseitin transformation should
   * also be satisfiable with the same assignment.
   */
  @Ignore
  @Test def testCNFUsingBDDs = check {
    forAll { (e: BExpr) =>
      val idMap = (e.identifiers.zipWithIndex map { case (id,i) => (id, i+1) }).toMap
      val cnf = e.toCNF(idMap)
      val bb = new BDDBuilder(idMap)
      val result = bb.mkBDD(cnf).biimpWith(bb.mkBDD(e))
      begin0(result != bb.zero)(result.free)
    }
  }
  
}