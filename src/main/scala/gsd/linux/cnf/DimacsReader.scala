package gsd.linux.cnf

import java.util.Scanner
import util.matching.Regex
import java.io.File

object DimacsReader {
  
  abstract sealed class DEq
  case class DLiteralEq(variable: Int, value: String) extends DEq
  case class DIntEq(variable: Int, value: Int) extends DEq
  case class DHexEq(variable: Int, value: String) extends DEq
  case class DStringEq(variable: Int, value: String) extends DEq
  case class DVarEq(variable: Int, value: Int) extends DEq

  case class DimacsProblem(numVars: Int, cnf: CNF)
  case class DimacsHeader(varMap: Map[Int, String],
                          eqMap: Map[Int, DEq],
                          generated: Set[Int],
                          firstGen: Int) {

    lazy val idMap: Map[String, Int] =
      (varMap map { case (v,id) => (id, v) }).toMap
    
  }

  // (?m) is multi-line mode -- ^ $ matches BOL and EOL
  val comment = "(?m)^c.*$".r.pattern

  val vardef  = """(?m)^c (\d+)(\$?) (\w+)(?: (.) (\d+) (.+))?$""".r
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
    val s = in.useDelimiter("""\r?\n""")

    val header = new collection.mutable.ListBuffer[(Int,Boolean,String)]
    val eqTuples = new collection.mutable.ListBuffer[(Int, DEq)]
    var firstGen = -1

    while (s hasNext comment) {
      val vardef(v, isGenSym, id, eqType, eqVar, eqValue) = s.nextLine
      val isGen = isGenSym != ""
      if (firstGen < 0 && isGen) firstGen = v.toInt

      header += ((v.toInt, isGen, id))
      eqType match {
        case "s" =>
          eqTuples += ((v.toInt, DStringEq(eqVar.toInt, eqValue)))
        case "h" =>
          eqTuples += ((v.toInt, DHexEq(eqVar.toInt, eqValue)))
        case "i" =>
          eqTuples += ((v.toInt, DIntEq(eqVar.toInt, eqValue.toInt)))
        case "v" =>
          eqTuples += ((v.toInt, DVarEq(eqVar.toInt, eqValue.toInt)))
        case null =>
          // Do nothing
        case _ =>
          sys.error("Unsupported equality type: " + eqType)
      }
    }

    DimacsHeader((header map { case (v, _, id) => (v, id) }).toMap,
        eqTuples.toMap,
        (header filter { case (_, isGen, _) => isGen } map { case (v, _, _) => v }).toSet,
        firstGen)
  }
}