package gsd.linux

import BDDBuilder._
import net.sf.javabdd.BDD

/**
 * TODO Make a BExpr instead of a BDD
 */
trait FMBDDBuilder extends BDDBuilder {
  type E = B2Expr 

  def mkBDD(in: FM[E]): BDD = {

    def _ithVar(f: Feature[E]) = factory ithVar idMap(f.name)

    def _mkBDD(p: Feature[E])(c: Node[E]) = c match {
     case f:MFeature[E] => _ithVar(f) biimpWith _ithVar(p)
     case f:OFeature[E] => _ithVar(f) impWith _ithVar(p)
     case g:Group[E] =>

       // Exclusions
       val exBDD = ((g.members zip g.members) filter {
         case (x,y) if x == y => false
         case _ => true
       } map { case (x,y) =>
         _ithVar(x) impWith _ithVar(x).not //FIXME mem leak?
       }).foldLeft(one){ _ andWith _ }

       // Disjunction of members
       val memBDD = (zero /: (g.members map _ithVar)) { _ orWith _ }

       g match {
         case _: OrGroup[E] => (_ithVar(p) impWith memBDD)
         case _: MutexGroup[E] => exBDD
         case _: XorGroup[E] => (_ithVar(p) impWith memBDD) andWith exBDD
       }

      //TODO process children and ctcs
    }

    (one /: (in.root.children map {
        _mkBDD(in.root)
      })){ _ andWith _ } andWith _ithVar(in.root)
  }


}