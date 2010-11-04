package gsd.linux.cnf

import collection.immutable.PagedSeq
import util.parsing.combinator.{PackratParsers, ImplicitConversions, RegexParsers}
import util.parsing.input.PagedSeqReader

case class CNFResult(ids: List[String],
                     generated: List[String],
                     idMap: Map[String, Int],
                     cnf: CNF) {
  val all = ids ::: generated
}

/**
 * Parser for CNF file format. Should probably switch to something standard, like
 * dimacs.
 */
object CNFParser extends RegexParsers with PackratParsers with ImplicitConversions {

  private abstract class Lit(val value: String)
  private case class PosLit(v: String) extends Lit(v)
  private case class NegLit(v: String) extends Lit(v)

  override protected val whiteSpace = """[ \t\f]""".r

  val nl = """[\r]?\n""".r
  val ids  = rep("@" ~> """\w+""".r <~ nl)
  val gens = rep("$" ~> """\w+""".r <~ nl)

  private val literal: PackratParser[Lit] =
    """[-]?\w+""".r ^^ { s =>
        if (s.charAt(0) == '-') NegLit(s.slice(1,s.length))
        else PosLit(s)
      }
  
  private val clause =
    rep(literal)

  private val cnfFile = ids ~ gens ~ rep(clause <~ nl) ^^ {
    case idents~gs~cnf =>
      val idMap = Map() ++ {
        (idents ++ gs).zipWithIndex map { case (id,v) => (id, v + 1) }
      }
      CNFResult(idents, gs, idMap,
        cnf map { clause => clause map { c => idMap(c.value) } })
  }

  def parseFile(file: String): CNFResult =
    succ(parseAll(cnfFile, new PagedSeqReader(PagedSeq fromFile file)))

  private def succ[A](p : ParseResult[A]) = p match {
    case Success(res,_) => res
    case x => error(x.toString)
  }

}
