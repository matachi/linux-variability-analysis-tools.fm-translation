package gsd.linux

import FMUtil._
import B2Expr._

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

    //Mapping from Config Id -> Cross-Tree Constraints
    val ctcs = Map() ++ {
      k.toAbstractKConfig.configs map { c =>
        c.id -> mkConfigConstraints(c)
      }
    }

    //Roots are features that are not present in the parentMap
    val roots = k.features.filter { hi.contains }

    def mkFeature(c: CSymbol): List[Feature[T]] = c match {
      case c: CConfig =>
        List(OFeature(c.id, BoolFeat, ctcs(c.id), c.children flatMap mkFeature))

      case _: CMenu =>
        List(MFeature(c.id, BoolFeat, Nil, c.children flatMap mkFeature))

      case _: CChoice => c.children flatMap mkFeature
    }

    FM(roots flatMap mkFeature, Nil) //TODO choices
  }

  /**
   * TODO clone of method in BooleanTranslation
   */
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
      case Not(e) =>
        println("Not: " + e)
        !t(e) //TODO Check this, may make a constraint stronger
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
   * Boolean translation may in fact, _strengthen_ the constraint. Thus, we
   * remove these antecedent expressions to retain our goal of relaxing
   * constraints in the Boolean model. The consequent expressions are unaffected
   * and thus, all expression are kept.
   *
   * Thus, the translation becomes:
   *   PROMPT | ((REV_DEP ++ DEFAULTS).||.keepAsAntecedent => C) &
   *           (C => ((REV_DEP ++ DEFAULTS).||)
   */
  def mkPresence(c: AConfig): T = {

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
        case h::tail => {
          h match {
            case Default(Yes, c) => negPrev & toBExpr(c)
            case Default(Mod, c) => negPrev & toBExpr(c)
            case Default(v: Value, c) => negPrev & toBExpr(c)
            case Default(e, c) => negPrev & toBExpr(c) & toBExpr(e)
          }
        } :: mkDefaultList(tail, h :: prev)
      } //end rest match
    } //end _mkDef
    
    def _negExprs(d: Default) = d.iv == No || d.iv == Literal("")

    def mkRevDeps(lst: List[KExpr]) = lst map toBExpr

    def mkDefaults(lst: List[Default]) =
      mkDefaultList(lst remove _negExprs, Nil)

    val antecedent = mkRevDeps(c.rev filter keepAsAntecedent) :::
                     mkDefaults(c.defs filter { case Default(e, c) =>
                       keepAsAntecedent(c) && keepAsAntecedent(e)
                     })
    val consequent = mkRevDeps(c.rev) ::: mkDefaults(c.defs)

    toBExpr(c.pro) |
      (antecedent.|| implies B2Id(c.id)) &
        (B2Id(c.id) implies consequent.||)
  }

  def mkConfigConstraints(c: AConfig): List[T] =
    mkPresence(c) :: Nil

}