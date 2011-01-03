package gsd.linux.cnf

import java.util.Scanner
import util.matching.Regex
import java.io.File

object DimacsReader {

  case class DimacsProblem(numVars: Int, cnf: CNF)
  case class DimacsHeader(varMap: Map[Int, String], generated: Set[Int]) {

    lazy val idMap: Map[String, Int] =
      (varMap map { case (v,id) => (id, v) }).toMap
    
  }

  // (?m) is multi-line mode -- ^ $ matches BOL and EOL
  val comment = "(?m)^c.*$".r.pattern
  val vardef  = """(?m)^c (\d+)(\$?) (.+)$""".r
  val problem = """^p cnf (\d+) (\d+)$""".r

  def readFile(file: String) = read(new Scanner(new File(file)))
  def readString(s: String) = read(new Scanner(s))

  def read(in: Scanner): DimacsProblem = {
    val s = in.useDelimiter(System.getProperty("line.separator"))

    while(s hasNext comment) s.nextLine //skip over comments

    val problem(numVars, numClauses) = s.nextLine

    val cnf =
      for (i <- 0 until numClauses.toInt) yield {
        val nums = s.nextLine.split(' ').filterNot { _ == "" }.toList map { _.toInt }
        assert(nums.last == 0, "input file not in dimacs format")
        nums.dropRight(1)
      }

    DimacsProblem(numVars.toInt, cnf.toList)
  }

  def readHeaderFile(file: String) = readHeader(new Scanner(new File(file)))
  def readHeaderString(s: String) = readHeader(new Scanner(s))

  def readHeader(in: Scanner): DimacsHeader = {
    val s = in.useDelimiter(System.getProperty("line.separator"))

    val header = new collection.mutable.ListBuffer[(Int,Boolean,String)]

    while (s hasNext comment) {
      val vardef(v, isGen, id) = s.nextLine
      header += ((v.toInt, isGen != "", id))
    }

    DimacsHeader(Map() ++ (header map { case (v, _, id) => (v, id) }),
        (header filter { case (_, isGen, _) => isGen } map { case (v, _, _) => v }).toSet)
  }
}