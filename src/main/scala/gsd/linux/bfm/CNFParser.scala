/*
 * This file is part of the Linux Variability Modeling Tools (LVAT).
 *
 * Copyright (C) 2010 Steven She <shshe@gsd.uwaterloo.ca>
 *
 * LVAT is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * LVAT is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LVAT.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

package gsd.linux

import util.parsing.combinator.{ImplicitConversions, RegexParsers}
import collection.immutable.PagedSeq
import util.parsing.input.PagedSeqReader

import CNF._

object CNFParser extends RegexParsers with ImplicitConversions {

  case class CNFData(cnf: CNF, ids: List[String], generated: List[String])

  override protected val whiteSpace = """[ \t\f]""".r

  val nl = """[\r]?\n""".r
  val ids  = rep("@" ~> """\w+""".r <~ nl)
  val gens = rep("$" ~> """\w+""".r <~ nl)

  val literal = """[-]?\w+""".r ^^
    { s =>
      if (s.charAt(0) == '-') NegLit(s.slice(1,s.length))
      else PosLit(s)
    }
  val clause = rep(literal)
  val cnfFile = ids ~ gens ~ rep(clause <~ nl) ^^ {
    case idents~gs~cnf => CNFData(cnf, idents, gs)
  }

  def parseClause(s: String) = succ(parseAll(clause, s))
  def parseCNFFile(file: String): CNFData =
    succ(parseAll(cnfFile,new PagedSeqReader(PagedSeq fromFile file)))

  private def succ[A](p : ParseResult[A]) = p match {
    case Success(res,_) => res
    case x => error(x.toString)
  }

}