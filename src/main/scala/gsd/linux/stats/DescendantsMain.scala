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
         case CMenu(_,true,children) =>
           children flatMap { child => w(child, depth) }
         case _ if depth > maxDepth => Nil
         case _ =>
           val desc = (sym.children flatMap { child => w(child, depth + 1) })
           ((sym, desc map { case ((s,_),_) =>  s }), depth) :: desc
      }

    val ts = w(k.root, 0)

    ts foreach { case ((sym, children), depth) =>
      println("%s %s".format(
        List.fill(depth)("    ").mkString + sym.id, "(" + children.size + ")"))
    }
  }


}