package gsd.linux.stats

import gsd.linux.cnf.{CNFBuilder, SATBuilder}
import org.sat4j.specs.ContradictionException
import gsd.linux._
import org.kiama.rewriting.Rewriter
import java.io.PrintStream

class ConstraintStatistics(val name: String,
                           override val ck: ConcreteKConfig) extends ASEStatistics(ck) {


  lazy val configsDeclaringProperties =
    ppk.allConfigs filter { !_.properties.isEmpty }

  private def mkSymbolsToId(in: List[CSymbol]): Map[String, Set[String]] =
    {
      in map {
        case c: CConfig =>
          (c.id, Rewriter.collects {
            case Id(s) => s
          }(c.properties ::: c.depends) ++ (c.sels map { _.id }))
        case c: CChoice =>
          (c.id, Rewriter.collects {
            case Id(s) => s
          }(c.properties))
        case c@CMenu(Prompt(_,e),false,_) =>
          (c.id, Rewriter.collects {
            case Id(s) => s
          }(e))
        case _ =>
          error("If conditions shouldn't appear here!")
      }
    }.toMap

  lazy val ppConfigsToIds: Map[String, Set[String]] =
    mkSymbolsToId(ppk.allConfigs)

  lazy val configsToIds: Map[String, Set[String]] =
    mkSymbolsToId(ck.allConfigs)

  lazy val ppChoiceToIds: Map[String, Set[String]] =
    mkSymbolsToId(ppk.choices)

  lazy val choiceToIds: Map[String, Set[String]] =
    mkSymbolsToId(ck.choices)

  lazy val ppMenusToIds: Map[String, Set[String]] =
    mkSymbolsToId(ppk.menus)

  lazy val menusToIds: Map[String, Set[String]] =
    mkSymbolsToId(ck.menus)

  lazy val ppSymbolsToIds =
    ppConfigsToIds ++ {
      ppChoiceToIds.zipWithIndex map {
        case ((_,v),i) => ("choice_" + (i+1)) -> v
      }
    } ++ {
      ppMenusToIds.zipWithIndex map {
        case ((_,v),i) => ("menu_" + (i+1)) -> v
      }
    }

}

object ConstraintStatistics {

  def isSubsumedBy(e1: KExpr, e2: KExpr): Boolean = {
    val t1 = TExpr.toTExpr(e1)
    val t2 = TExpr.toTExpr(e2)

    val (b11, b12) = t1.toBExpr
    val (b21, b22) = t2.toBExpr

    // TODO ignoring b12 and b22
    val b = (b11 & !b21)
    val idmap = (b.identifiers.zipWithIndex map { case (id,i) => (id, i+1) }).toMap
    val cnf = CNFBuilder.toCNF(b, idmap)

    try {
      val sat = new SATBuilder(cnf, idmap.size)
      !sat.isSatisfiable
    }
    catch {
      case _: ContradictionException => true
      case e => throw e
    }
  }

}
