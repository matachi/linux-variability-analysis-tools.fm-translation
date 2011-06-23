package gsd.linux.stats

import gsd.linux._

object DescendantsMain {

  def main(args: Array[String]) {
    if (args.size < 2) {
      System.err println "Usage: DescendantsMain <kconfig-extract> <max-depth>"
      System exit 1
    }

    val k = KConfigParser.parseKConfigFile(args(0))
    val maxDepth = args(1).toInt

    def w(sym: CSymbol, depth: Int): List[((CSymbol, List[CSymbol]), Int)] =
      sym match {
          // Ignore if-conditions
         case CMenu(_,children) =>
           children flatMap { child => w(child, depth) }

         case _ =>
           val desc = (sym.children flatMap { child => w(child, depth + 1) })
           ((sym, desc map { case ((s,_),_) =>  s }), depth) :: desc
      }

    val ts = w(k.root, 0)

    ts foreach {
      case ((sym, children), depth) if depth <= maxDepth =>

      val name = sym match {
        case CConfig(_,_,_,_,prompts,_,_,_,_,_) if !prompts.isEmpty =>
          prompts.head.text
        case _ => sym.id
      }

      val symType = sym match {
        case CMenu(_,_) => "menu"
        case CConfig(_,true,ktype,_,_,_,_,_,_,_) => "menuconfig"
        case CConfig(_,false,ktype,_,_,_,_,_,_,_) => "config"
        case _: CChoice => "choice"
      }

      println("%s%s (%s) %s".format(
        List.fill(depth)("    ").mkString,
        name,
        symType,
        "(" + children.size + ")"))

      case _ =>
        // do nothing
    }
  }


}