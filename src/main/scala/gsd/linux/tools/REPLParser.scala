package gsd.linux.tools

import java.io.PrintStream
import util.parsing.combinator.{JavaTokenParsers, RegexParsers, PackratParsers}

/**
* TODO
*/
class REPLParser extends RegexParsers with JavaTokenParsers with PackratParsers {

  val num: Parser[Int] = "[0-9]+".r ^^ { _.toInt }
  val id: Parser[String] = "\\w+".r

  val filename: Parser[String] = stringLiteral

  def descCmd(out: PrintStream) = //TODO
    "descendants" ~> opt("\\w".r) ~ opt(num) ^^ {
      case Some(node) ~ None =>
      case None ~ Some(depth) =>
      case Some(node) ~ Some(depth) =>
      case None ~ None =>
        failure("descendants requires at least one argument")
    }

  def saveCmd(out: PrintStream): PackratParser[Unit] =
    "save" ~> ((filename ^^ { new PrintStream(_) }) into cmd)

  def cmd(out: PrintStream): PackratParser[Unit] =
    saveCmd(out)

  def process(line: String): Unit =
    parseAll(cmd(System.out), line) match {
      case Success(res,in) if in.atEnd =>
      case Success(res,in) =>
        println("[Warn] Extraneous input: " + in)
      case fail =>
        println(fail)
    }

}

