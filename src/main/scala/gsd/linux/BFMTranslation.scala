/*
 * This file is part of the Linux Variability Modeling Tools (LVAT).
 *
 * Copyright (C) 2010 Steven She <shshe@gsd.uwaterloo.ca>
 *
 * LVAT is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * LVAT is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LVAT.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

package gsd.linux

import FMUtil._
import B2Expr._
import collection.immutable.TreeSet

object BFMTranslation {

  private type T = B2Expr

  /**
   * The algorithm uses two maps for constructing the hierarchy:
   *     1. Hierarchy Map - A map of a feature to its immediate parent.
   *     2. Config Map - A map of a feature to its closest ancestor config.
   *
   * The SAT solver only contains configs since only configs can participate in
   * constraints.
   *
   * TODO factor out lots of boilerplate code shared with TFMTranslation.
   */
  def mkFeatureModel(hi: Hierarchy, k: ConcreteKConfig) = {

    import ExprUtil._

    //Mapping from Config Id -> Cross-Tree Constraints
    val configCTCs = Map() ++ {
      k.toAbstractKConfig.configs map { c =>
        c.id -> removeTrue(rewriteExpr(mkConfigConstraints(c)))
      }
    }

    val choiceCTCs = Map() ++ {
      k.toAbstractKConfig.choices map { c =>
        c.memIds -> removeTrue(rewriteExpr(mkChoiceConstraints(c)))
      }
    }

    //Configs that are referenced in configs but not declared
    val freeVars = k.identifiers.toList -- (k.allConfigs map { _.id })

    def mkFeature(c: CSymbol): Node[T] = c match {
      case c: CConfig =>
        OFeature(c.id, BoolFeat, configCTCs(c.id), c.children map mkFeature)

      case _: CMenu =>
        //FIXME Remove quotes for menus
        MFeature(c.id.substring(1, c.id.length - 1), BoolFeat, Nil, c.children map mkFeature)

      //TODO factor out function
      case o: CChoice => o match {
        case CChoice(Prompt(name,_),true,true,_,cs) =>
          XorGroup(name, c.children map mkFeature, choiceCTCs(cs map { _.id }))
        case CChoice(Prompt(name,_),true,false,_,cs) =>
          MutexGroup(name, c.children map mkFeature, choiceCTCs(cs map { _.id }))
        case CChoice(Prompt(name,_),false,true,_,cs) =>
          OrGroup(name, c.children map mkFeature, choiceCTCs(cs map { _.id }))
        case CChoice(Prompt(name,_),false,false,_,cs) =>
          OptGroup(name, c.children map mkFeature, choiceCTCs(cs map { _.id }))
      }
    }

    def mkFreeVar(s: String): OFeature[T] =
      OFeature(s, BoolFeat, Nil, Nil)

    val root = mkFeature(k.root)
    FM(mkFeature(k.root) :: (freeVars map mkFreeVar))
  }

  def toBExpr(in: KExpr): B2Expr = {
      def t(e: KExpr): B2Expr = e match {
      case No => B2False
      case Mod | Yes => B2True

      case Literal(_) | KHex(_) | KInt(_) => B2False

      case Eq(Id(n), Yes) => B2Id(n)
      case Eq(Id(n), Mod) => B2Id(n)
      case Eq(Id(n), No) => !B2Id(n)
      case Eq(Id(n), Literal("")) => !B2Id(n)
      case Eq(Id(n), Literal(_)) => B2Id(n)
      case Eq(Id(n), KHex(_)) => B2Id(n)
      case Eq(Id(n), KInt(_)) => B2Id(n)
      case Eq(Id(x), Id(y)) => B2Id(x) iff B2Id(y)

      case NEq(Id(n), Yes) => B2True //Can be M or N -- translates to T or F
      case NEq(Id(n), Mod) => B2True //Can be M or N -- translates to T or F
      case NEq(Id(n), No) => B2Id(n)
      case NEq(x, y) => !t(Eq(x, y))

      case And(x, y) => t(x) & t(y)
      case Or(x, y) => t(x) | t(y)

      case Not(Mod) | Not(No) | Not(Yes) => B2True //translates to T or F
      case Not(Id(n)) => !B2Id(n) //This case is OK
      case Not(e) =>
        System.err.println("Unexpected Negation in Kconfig: " + e)
        !t(e)
      case Id(n) => B2Id(n)

      case e => error("Unexpected expression: " + e + ": " + e.getClass)
    }
    t(in)
  }


  /**
   * The presence expression of a config is constructed using three components:
   *   (1) Prompt Expression, (2) Reverse Dependencies and (3) Defaults.
   *
   * The high-level translation for a config C is as follows:
   *   PROMPT | ((REV_DEP ++ DEFAULTS).|| <=> C)
   *
   * There is a bi-implication in the translation. As a result,
   * when an expression appears as the antecedent an implication, the
   * Boolean translation may _strengthen_ the constraint. Thus, we
   * remove these antecedent expressions to relaxing constraints in the Boolean
   * model. The consequent expressions are unaffected and all expression are kept.
   *
   * The translation becomes:
   *   PROMPT | ((REV_DEP ++ DEFAULTS).||.keepAsAntecedent => C) &
   *           (C => ((REV_DEP ++ DEFAULTS).||)
   */
  def mkPresence(c: AConfig): List[T] = {

    /**
     * We remove KExpr that contains = or a reference to != y or != m. These
     * KExpr, when used as the antecedent of an implication cause our constraint
     * to become _stronger_ in the Boolean model.
     *
     * TODO there may be more cases than this
     */
    def keepAsAntecedent(e: KExpr): Boolean =
      KExprRewriter.count {
        case _:Eq | NEq(_,Yes) | NEq(_,Mod) => 1
      }(e) == 0

    /**
     * Constructs a list of BExpr (to be used in an OR) from a list of Defaults.
     */
    def mkDefaultList(rest: List[Default], prev: List[Default]): List[T] = {
      val negPrev =
        ((B2True: B2Expr) /: prev){ (x,y) => x & !toBExpr(y.cond) }

      rest match {
        case Nil => Nil
        case (h@Default(e,c))::tail => {
          negPrev & toBExpr(c) & toBExpr(e)
        } :: mkDefaultList(tail, h :: prev)
      } //end rest match
    } //end mkDefaultList


    def _negExprs(d: Default) = d.iv == No || d.iv == Literal("")

    def mkRevDeps(lst: List[KExpr]) = lst map toBExpr

    def mkDefaults(lst: List[Default]) =
      mkDefaultList(lst remove _negExprs, Nil)

    val antecedent = mkRevDeps(c.rev filter keepAsAntecedent) :::
                     mkDefaults(c.defs filter { case Default(e, c) =>
                       keepAsAntecedent(c) && keepAsAntecedent(e)
                     })
    val consequent = mkRevDeps(c.rev) ::: mkDefaults(c.defs)

    (toBExpr(c.pro) | (B2Id(c.id) implies consequent.||)) ::
      (antecedent map { ant => toBExpr(c.pro) | (ant implies B2Id(c.id)) })
  }

  /**
   * The inherited constraints are those carried over from Menus and Choices.
   * Since we maintain both Menus and Choices, we can safely ignore this for
   * our translated model.
   */
  def mkInherited(c: AConfig): List[T] = Nil

  def mkChoiceConstraints(c: AChoice): List[T] =
    if (c.vis == Yes) Nil
    else List(toBExpr(c.vis))

  def mkConfigConstraints(c: AConfig): List[T] =
    mkPresence(c) ::: mkInherited(c)


}