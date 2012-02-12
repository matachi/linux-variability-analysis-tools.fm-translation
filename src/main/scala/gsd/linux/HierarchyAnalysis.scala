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
   * 
   * These do not include dead features that are children of the root.
   *
   * Only the immediate child that violates the hierarchy constraints is
   * recorded.
   */
  def findViolatingConfigs(k: ConcreteKConfig,
                           sat: SATBuilder,
                           idMap: Map[String, Int]): List[CConfig] = {

    def traverse(par: Option[CConfig])(curr: CSymbol): List[CConfig] = {
      val violating = par match {
        case Some(p) =>
          // Find configs that violate hierarchy
          curr.children collect {
            case c: CConfig if sat.isSatisfiable(List(idMap(c.name), -idMap(p.name))) => c
          }

        // Features at root don't have a closest config as parent, ignore
        case None =>
          Nil
      }

      curr match {
        case c: CConfig =>
          // If the current node is a config, set it to the closest parent
          violating ::: (curr.children flatMap traverse(Some(c)))
        case _ =>
          // Otherwise, we use the current closest parent
          violating ::: (curr.children flatMap traverse(par))
      }
    }

    traverse(None)(k.root)
  }

}