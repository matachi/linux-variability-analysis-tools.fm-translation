package gsd.linux.stats

import gsd.linux._
import java.io.PrintStream
import tools.ConstraintCheckerMain

/**
 * Generates text output of the hierarchy structure up to a specified max depth.
 */
object DescendantsMain {

  def main(args: Array[String]) {
    if (args.size < 2) {
      System.err println "Usage: DescendantsMain <kconfig-extract> <max-depth> [<output_file>]"
      System exit 1
    }

    val k = KConfigParser.parseKConfigFile(args(0))
    val maxDepth = args(1).toInt
    val out = if (args.size > 2) new PrintStream(args(2)) else System.out

    def w(sym: CSymbol, depth: Int): List[((CSymbol, List[CSymbol]), Int)] =
      sym match {
        case x if depth > maxDepth =>
          Nil

        // Ignore virtual nodes
        case x if x.isVirtual =>
          x.children flatMap { c => w(c, depth) }

        case _ =>
          val desc = (sym.children flatMap { child => w(child, depth + 1) })
          ((sym, desc map { case ((s,_),_) =>  s }), depth) :: desc
      }

    val ts = w(k.root, 0)

    ts foreach {
      case ((sym, children), depth) =>

      val name = sym match {
        case CConfig(_,name,_,_,_,prompts,_,_,_,_,_) =>
          if (prompts.isEmpty) name
          else prompts.head.text
        case CMenu(_, Prompt(name,_),_) => name
        case CChoice(_,Prompt(name,_),_,_,_,_) => name
        case _ => sys.error("this should never occur.")
      }

      val symType = sym match {
        case _:CMenu => "menu"
        case CConfig(_,_,true,ktype,_,_,_,_,_,_,_) => "menuconfig"
        case CConfig(_,_,false,ktype,_,_,_,_,_,_,_) => "config"
        case _: CChoice => "choice"
        case _ => sys.error("This should never occur.")
      }


      out.println("%s%s (%s) %s".format(
        List.fill(depth)("    ").mkString,
        name,
        symType,
        "(" + children.size + ")"))

      case _ =>
        // do nothing
    }
  }


}