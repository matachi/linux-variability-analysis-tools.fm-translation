///*
// * This file is part of the Linux Variability Modeling Tools (LVAT).
// *
// * Copyright (C) 2010 Steven She <shshe@gsd.uwaterloo.ca>
// *
// * LVAT is free software: you can redistribute it and/or modify it under
// * the terms of the GNU Lesser General Public License as published by the
// * Free Software Foundation, either version 3 of the License, or (at your
// * option) any later version.
// *
// * LVAT is distributed in the hope that it will be useful, but WITHOUT ANY
// * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
// * more details.
// *
// * You should have received a copy of the GNU Lesser General Public License
// * along with LVAT.  (See files COPYING and COPYING.LESSER.)  If not, see
// * <http://www.gnu.org/licenses/>.
// */
//
//package gsd.linux
//
//import FMTranslationUtil._
//import BExpr._
//import TypeFilterList._
//
//import org.kiama.rewriting.Rewriter._
//
//object BFMTranslation {
//
//  private type T = BExpr
//
//  /**
//   * The algorithm uses two maps for constructing the hierarchy:
//   *     1. Hierarchy Map - A map of a feature to its immediate parent.
//   *     2. Config Map - A map of a feature to its closest ancestor config.
//   *
//   * The SAT solver only contains configs since only configs can participate in
//   * constraints.
//   *
//   * TODO factor out lots of boilerplate code shared with TFMTranslation.
//   */
//  def mkFeatureModel(hi: Hierarchy, k: ConcreteKConfig) = {
//
//    import BExprUtil._
//
//    //Mapping from Config Id -> Cross-Tree Constraints
//    val configCTCs = Map() ++ {
//      k.toAbstractKConfig.configs map { c =>
//        c.id -> removeTrue(rewriteExpr(mkConfigConstraints(c)))
//      }
//    }
//
//    val choiceCTCs = Map() ++ {
//      k.toAbstractKConfig.choices map { c =>
//        c.memIds -> removeTrue(rewriteExpr(mkChoiceConstraints(c)))
//      }
//    }
//
//    //Configs that are referenced in configs but not declared
//    val freeVars =
//      k.identifiers.toList filterNot ((k.allConfigs map { _.id }) contains)
//
//    def mkConfig(c: CConfig): OFeature[T] =
//      OFeature(c.id, BoolFeat, configCTCs(c.id), c.children map mkFeature)
//
//    //FIXME Remove quotes for menus
//    def mkMenu(m: CMenu): MFeature[T] =
//      MFeature(m.id.substring(1, m.id.length - 1), BoolFeat, Nil, m.children map mkFeature)
//
//    def mkFeature(c: CSymbol): Node[T] = c match {
//      case c: CConfig => mkConfig(c)
//      case m: CMenu => mkMenu(m)
//
//      //Assuming choices have only configs as children. Holds for Linux, not
//      //sure about items Kconfig projects.
//      case o: CChoice =>
//        val configs = c.children.typeFilter[CConfig]
//        assert(configs.size == c.children.size, "Choice members aren't all configs!")
//
//        o match {
//          case CChoice(Prompt(name,_),true,true,_,cs) =>
//            XorGroup(name, configs map mkConfig, choiceCTCs(cs map { _.id }))
//          case CChoice(Prompt(name,_),true,false,_,cs) =>
//            MutexGroup(name, configs map mkConfig, choiceCTCs(cs map { _.id }))
//          case CChoice(Prompt(name,_),false,true,_,cs) =>
//            OrGroup(name, configs map mkConfig, choiceCTCs(cs map { _.id }))
//          case CChoice(Prompt(name,_),false,false,_,cs) =>
//            OptGroup(name, configs map mkConfig, choiceCTCs(cs map { _.id }))
//        }
//    }
//
//    def mkFreeVar(s: String): OFeature[T] =
//      OFeature(s, BoolFeat, Nil, Nil)
//
//    val root = mkMenu(k.root) match {
//      case MFeature(name, t, ctcs, cs) =>
//        MFeature(name, t, ctcs, cs ::: (freeVars map mkFreeVar))
//    }
//
//    FM(root)
//  }
//
//  def toBExpr(in: KExpr): BExpr = {
//      def t(e: KExpr): BExpr = e match {
//      case No => BFalse
//      case Mod | Yes => BTrue
//
//      case Literal(_) | KHex(_) | KInt(_) => BFalse
//
//      case Eq(Id(n), Yes) => BId(n)
//      case Eq(Id(n), Mod) => BId(n)
//      case Eq(Id(n), No) => !BId(n)
//      case Eq(Id(n), Literal("")) => !BId(n)
//      case Eq(Id(n), Literal(_)) => BId(n)
//      case Eq(Id(n), KHex(_)) => BId(n)
//      case Eq(Id(n), KInt(_)) => BId(n)
//      case Eq(Id(x), Id(y)) => BId(x) iff BId(y)
//
//      case NEq(Id(n), Yes) => BTrue //Can be M or N -- translates to T or F
//      case NEq(Id(n), Mod) => BTrue //Can be M or N -- translates to T or F
//      case NEq(Id(n), No) => BId(n)
//      case NEq(x, y) => !t(Eq(x, y))
//
//      case And(x, y) => t(x) & t(y)
//      case Or(x, y) => t(x) | t(y)
//
//      case Not(Mod) | Not(No) | Not(Yes) => BTrue //translates to T or F
//      case Not(Id(n)) => !BId(n) //This case is OK
//      case Not(e) =>
//        System.err.println("Unexpected Negation in Kconfig: " + e)
//        !t(e)
//      case Id(n) => BId(n)
//
//      case e => error("Unexpected expression: " + e + ": " + e.getClass)
//    }
//    t(in)
//  }
//
//
//  /**
//   * The presence expression of a config is constructed using three components:
//   *   (1) Prompt Expression, (2) Reverse Dependencies and (3) Defaults.
//   *
//   * The high-level translation for a config C is as follows:
//   *   PROMPT | ((REV_DEP ++ DEFAULTS).|| <=> C)
//   *
//   * There is a bi-implication in the translation. As a result,
//   * when an expression appears as the antecedent an implication, the
//   * Boolean translation may _strengthen_ the constraint. Thus, we
//   * remove these antecedent expressions to relaxing constraints in the Boolean
//   * model. The consequent expressions are unaffected and all expression are kept.
//   *
//   * The translation becomes:
//   *   PROMPT | ((REV_DEP ++ DEFAULTS).||.keepAsAntecedent => C) &
//   *           (C => ((REV_DEP ++ DEFAULTS).||)
//   */
//  def mkPresence(c: AConfig): List[T] = {
//
//    /**
//     * We remove KExpr that contains = or a reference to != y or != m. These
//     * KExpr, when used as the antecedent of an implication cause our constraint
//     * to become _stronger_ in the Boolean model.
//     *
//     * TODO there may be more cases than this
//     */
//    def keepAsAntecedent(e: KExpr): Boolean =
//      count {
//        case _:Eq | NEq(_,Yes) | NEq(_,Mod) => 1
//      }(e) == 0
//
//    /**
//     * Constructs a list of BExpr (to be used in an OR) from a list of Defaults.
//     */
//    def mkDefaultList(rest: List[Default], prev: List[Default]): List[T] = {
//      val negPrev =
//        ((BTrue: BExpr) /: prev){ (x,y) => x & !toBExpr(y.cond) }
//
//      rest match {
//        case Nil => Nil
//        case (h@Default(e,c))::tail => {
//          negPrev & toBExpr(c) & toBExpr(e)
//        } :: mkDefaultList(tail, h :: prev)
//      } //end rest match
//    } //end mkDefaultList
//
//
//    def _negExprs(d: Default) = d.iv == No || d.iv == Literal("")
//
//    def mkRevDeps(lst: List[KExpr]) = lst map toBExpr
//
//    def mkDefaults(lst: List[Default]) =
//      mkDefaultList(lst filterNot _negExprs, Nil)
//
//    val antecedent = mkRevDeps(c.rev filter keepAsAntecedent) :::
//                     mkDefaults(c.defs filter { case Default(e, c) =>
//                       keepAsAntecedent(c) && keepAsAntecedent(e)
//                     })
//    val consequent = mkRevDeps(c.rev) ::: mkDefaults(c.defs)
//
//    (toBExpr(c.pro) | (BId(c.id) implies consequent.||)) ::
//      (antecedent map { ant => toBExpr(c.pro) | (ant implies BId(c.id)) })
//  }
//
//  /**
//   * The inherited constraints are those carried over from Menus and Choices.
//   * Since we maintain both Menus and Choices, we can safely ignore this for
//   * our translated model.
//   */
//  def mkInherited(c: AConfig): List[T] = Nil
//
//  def mkChoiceConstraints(c: AChoice): List[T] =
//    if (c.inherited == Yes) Nil
//    else List(toBExpr(c.inherited))
//
//  def mkConfigConstraints(c: AConfig): List[T] =
//    mkPresence(c) ::: mkInherited(c)
//
//
//}