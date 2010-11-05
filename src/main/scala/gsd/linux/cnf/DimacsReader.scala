package gsd.linux.cnf

import java.util.Scanner
import util.matching.Regex
import java.io.File

object DimacsReader {

  case class DimacsProblem(numVars: Int, cnf: CNF)
  case class DimacsHeader(varMap: Map[Int, String], generated: Set[Int])

  val comment = """c (\d+)(\$?) (.+)""".r
  val problem = """p cnf (\d+) (\d+)""".r

  def readFile(file: String) = read(new Scanner(new File(file)))
  def readString(s: String) = read(new Scanner(s))

  def read(in: Scanner): DimacsProblem = {
    val s = in.useDelimiter("(\r?\n)+")
    s.skip("(c .*\n)*") // Skip over comments

    val problem(numVars, numClauses) = s.nextLine

    val cnf =
      for (i <- 0 until numClauses.toInt) yield
        s.nextLine.split(' ').toList map { _.toInt }

    DimacsProblem(numVars.toInt, cnf)
  }

  def readHeaderString(s: String) = readHeader(new Scanner(s))

  def readHeader(in: Scanner): DimacsHeader = {
    val s = in.useDelimiter("(\r?\n)+")
    
    def doComments: List[(Int,Boolean,String)] =
      if (s hasNext (comment pattern))
        (s next (comment pattern)) match {
          case comment(v, isGen, id) =>
            (v.toInt, isGen != "", id) :: doComments
        }
      else Nil

    val header = doComments
    
    DimacsHeader(Map() ++ (header map { case (v, _, id) => (v, id) }),
        (header filter { case (_, isGen, _) => isGen } map { case (v, _, _) => v }).toSet)
  }

}