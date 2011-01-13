package gsd.linux.stats

import java.io.PrintStream
import gsd.linux.{CSymbol, KConfigParser}

abstract class StatisticsMain {
  type Stats = ConstraintStatistics with VisibilityStatistics

  def execute(stats: Stats)(out : PrintStream): Unit

  def main(args : Array[String]) = {
    if (args.size < 1) {
      System.err.println("Parameters: <exconfig file> <output file>")
      System exit 1
    }

    implicit val out: PrintStream =
      if (args.size > 1) new PrintStream(args(1))
      else System.out

    val extract = KConfigParser.parseKConfigFile(args head)
    val name = new java.io.File(args(0)).getName
    val stats = new ConstraintStatistics(
      name.take(name.lastIndexOf('.')), extract) with VisibilityStatistics
    execute(stats)(out)

    out.flush
    out.close
  }
}

object IdsPerConfigMain extends StatisticsMain {

  def execute(stats: Stats)(out: PrintStream) {
    stats.configsToIds foreach { case (config, ids) =>
      out.println(config + "," + ids.size)
    }
  }

}

object IdsPerConfigPPMain extends StatisticsMain {

  def execute(stats: Stats)(out: PrintStream) {
    stats.ppConfigsToIds foreach { case (config, ids) =>
      out.println(config + "," + ids.size)
    }
  }

}

object IdsPerSymbolPPMain extends StatisticsMain {

  def execute(stats: Stats)(out: PrintStream) {
    stats.ppConfigsToIds foreach { case (s, ids) =>
      out.println("%s,%d,%s".format(s, ids.size, "config"))
    }
    stats.ppMenusToIds.zipWithIndex map {
      case ((_,v),i) => ("menu_" + (i+1)) -> v
    } foreach { case (s, ids) =>
      out.println("%s,%d,%s".format(s, ids.size, "menu"))
    }
    stats.ppChoiceToIds.zipWithIndex map {
      case ((_,v),i) => ("choice_" + (i+1)) -> v
    } foreach { case (s, ids) =>
      out.println("%s,%d,%s".format(s, ids.size, "choice"))
    }
  }

}

object ConstraintDetailsMain extends StatisticsMain {

  def execute(stats: Stats)(out: PrintStream) {
    def compare(x: (String, Set[String]), y: (String, Set[String])) =
      if (x._2.size > y._2.size ) x else y

//    val top = sort(stats.configsToIds) take 3

//    out.println("Regular")
//    top foreach { case (id, refs) =>
//      out.println(("%s (%d)").format(id, refs.size))
//    }
    out.println("# features: %d".format(stats.features.size))
    out.println

    val numInCTCs: Int = (stats.ppSymbolsToIds filter { case (_,v) => v.size > 0 }).size
    out.println("# w/CTCs:  %d".format(numInCTCs))
    out.println("%% w/CTCs: %.2f".format(numInCTCs.toDouble / stats.features.size))

    // symbol with the most ids referenced
    val (topSymbol, topIds) = stats.ppSymbolsToIds reduceLeft compare
    out.println("Most Ids Referenced:")
    out.println(("  %s (%d)").format(topSymbol, topIds.size))
  }

}

object RepresentationCSVMain extends StatisticsMain {

  def execute(stats: Stats)(out: PrintStream) {
    out.println("%d,%s,%s".format(
      stats.boolType.size, "bool", stats.name))
    out.println("%d,%s,%s".format(
      stats.tristateType.size, "tristate", stats.name))
    out.println("%d,%s,%s".format(
      stats.intConfigs.size, "int", stats.name))
    out.println("%d,%s,%s".format(
      stats.hexConfigs.size, "hex", stats.name))
    out.println("%d,%s,%s".format(
      stats.stringConfigs.size, "string", stats.name))
    out.println("%d,%s,%s".format(
      stats.menus.size, "menu", stats.name))
    out.println("%d,%s,%s".format(
      stats.choices.size, "choice", stats.name))
  }
}

object ChoiceSizeCSVMain extends StatisticsMain {

  def execute(stats: Stats)(out: PrintStream) {
    stats.choices foreach { c =>
      out.println("%d,%d,%s,%s".format(
        c.children.size, stats.configs.size, "size", stats.name))
    }
  }

}

object VisibilityCSVMain extends StatisticsMain {

  def execute(stats: Stats)(out: PrintStream) {
    out.println("%d,%s,%s".format(
      stats.configsWithVisConds.size, "cond-vis", stats.name))
    out.println("%d,%s,%s".format(
      stats.configsWithMultiplePrompts.size, "cond-vis-multi-prompts", stats.name))
    out.println("%d,%s,%s".format(
      stats.configsWithNoVisConds.size, "always-vis", stats.name))
    out.println("%d,%s,%s".format(
      stats.configsWithUncondDerived.size, "no-vis", stats.name))
  }
}

object DefaultDetailsMain extends StatisticsMain {

  def execute(stats: Stats)(out: PrintStream) {
    // TODO categories can overlap
    out.println("Soft Defs:       %4d / %4d".format(
      stats.configsWithTrueDefs.size, stats.allConfigs.size))
    out.println("Cond. Derived:   %4d / %4d".format(
      stats.configsWithCondDerived.size, stats.allConfigs.size))

    out.println
    out.println("Reverse Deps:    %4d / %4d".format(
      stats.configsWithRevDeps.size, stats.allConfigs.size))
  }
}
