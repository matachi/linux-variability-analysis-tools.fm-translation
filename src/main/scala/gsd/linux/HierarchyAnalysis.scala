package gsd.linux

import cnf.SATBuilder
import TypeFilterList._

/**
 * Taken from the old FMTranslationUtil
 */
object HierarchyAnalysis {

  /**
   * @param pm A map from configs to their parent (could be a config, menu or choice).
   * @return a list of the ancestors of child beginning with its immediate parent.
   */
  def mkAncestorList(pm: Map[CSymbol, CSymbol])(child: CSymbol): List[CSymbol] =
    pm.get(child) match {
      case Some(parent) => parent :: mkAncestorList (pm)(parent)
      case None => Nil
    }

  /**
   * Returns a list of configs that can be present when it's parent is not.
   */
  def findViolatingConfigs(k: ConcreteKConfig,
                           sat: SATBuilder,
                           idMap: Map[String, Int]): List[CConfig] = {

    //A map from all features to their closest ancestor that's a Config
    val configMap = Hierarchy.mkConfigMap(k)

    // A child config is violating hierarchy if it can be present
    // when it's parent is not. i.e. SAT(!(child -> parent))
    // dead features return false since we call SAT(child,...)
    configMap filter {
      case (child, par) =>
        sat.isSatisfiable(List(idMap(child.name), -idMap(par.name)))
    } map { _._1 } toList
  }

}