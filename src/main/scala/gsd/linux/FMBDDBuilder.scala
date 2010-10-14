package gsd.linux

import BDDBuilder._
import net.sf.javabdd.BDD

/**
 * TODO Make a BExpr instead of a BDD
 */
trait FMBDDBuilder extends BDDBuilder {
  type E = B2Expr 

  def mkAndWithAll(lst: List[BDD]) = (one /: lst){ _ andWith _ }

  def mkBDD(in: FM[E]): BDD = {

    def _ithVar(f: Feature[E]) = factory ithVar idMap(f.name)

    def _mkBDD(p: Feature[E])(c: Node[E]): BDD = {
      c match {
        case f:MFeature[E] =>
          (_ithVar(f) biimpWith _ithVar(p)) andWith
            mkAndWithAll(f.children map { _mkBDD(f) })

        case f:OFeature[E] =>
          (_ithVar(f) impWith _ithVar(p)) andWith
            mkAndWithAll(f.children map { _mkBDD(f) })

        case g:Group[E] =>
          // exclusions
          val exBDD = ((g.members zip g.members) filter {
            case (x,y) if x == y => false
            case _ => true
          } map { case (x,y) =>
            _ithVar(x) impWith _ithVar(x).not // FIXME mem leak?
          }).foldLeft(one){ _ andWith _ }

          // disjunction of members
          val memBDD = (zero /: (g.members map _ithVar)) { _ orWith _ }

          {
            g match {
              case _: OrGroup[E] => (_ithVar(p) impWith memBDD)
              case _: MutexGroup[E] => exBDD
              case _: XorGroup[E] => (_ithVar(p) impWith memBDD) andWith exBDD
              case _: OptGroup[E] => one
            }
          } andWith mkAndWithAll(g.members map { _mkBDD(p) })
      }
    } andWith mkAndWithAll(c.constraints map mkBDD) // Cross-Tree Constraints

    (one /: (in.root.children map {
        _mkBDD(in.root)
      })){ _ andWith _ } andWith _ithVar(in.root)
  }


}