package gsd.linux

import TExpr._

/**
 * Input: AbstractKConfig
 *
 * TODO Maybe include boolean operators as well.
 */
object TExprTranslation {

  def mkDefaults(c: AConfig): List[TExpr] = {

    def _mkExprs(rest: List[Default], prev: List[Default]): List[TExpr] = {
      val negatedPrev = ((TYes: TExpr) /: prev) { (x, y) => x & !TBool(y.cond) }

      rest match {
        case Nil => Nil
        case (head@Default(e,cond))::tail =>
          //If the condition is 'Yes', then the config takes on the value of the
          //evaluated condition.
          val expr:TExpr =
            if (e == Yes && c.ktype == KTriType) cond
            else if (e == Yes && c.ktype == KBoolType) TBool(cond)
            else e
          
          (TBool(c.pro) | (negatedPrev & TBool(cond) &
            (TId(c.id) eqs (expr | c.rev.toTExprs.||)))) ::
              _mkExprs(tail, head :: prev)
      }

    }

    _mkExprs(c.defs, Nil)
  }

  /**
   * Returns an expression modeling the upper and lower bounds.
   * Reverse dependencies model the lower bound, while the prompt
   * models the upper bound.
   *
   */
  def mkBounds(c: AConfig): TExpr =
    (TId(c.id) >= c.rev.toTExprs.||) & (TId(c.id) <= c.pro)


  /**
   * TODO No checks on string, int and hex.
   */
  def mkType(c: AConfig): Option[TExpr] = c.ktype match {
    case KBoolType => Some((TId(c.id) eqs TNo) | (TId(c.id) eqs TYes))
    case _ => None
  }

  /**
   * TODO ctcs
   */
  def mkChoiceConstraints(c: AChoice): List[TExpr] = {

    val exclusions = Combinations.choose(2, c.memIds).map {
      case x::y::Nil => !TId(x) | !TId(y)
      case _ => error("Unreachable.")
    }

    //FIXME optional and visibility
    c match {
      case AChoice(vis, true, true, memIds) =>
        val boolC = memIds.map { m => TId(m) eqs TYes }.||
        boolC :: exclusions
      case AChoice(vis, false, true, memIds) =>
        val triC = memIds.map { m => TId(m) >= TMod }.||
        val memDisj = memIds.map { m => TId(m) eqs TYes }.||
        val condEx = exclusions map { memDisj implies _ }
        triC :: condEx
      case _ => //TODO
        Nil
    }
  }


  def mkConfigConstraints(c: AConfig): List[TExpr] =
    mkBounds(c) :: mkType(c).toList ::: mkDefaults(c)

}

object KConfigUtil {

  /**
   * Chunks defaults such that each list of defaults has the same consecutive
   * default values (ie. Default.iv)
   *
   * TODO move into separate object
   */
  def chunkDefaults(defs: List[Default]): List[List[Default]] = defs match {
    case Nil => Nil
    case Default(iv, _) :: _ =>
      val (same, rest) = defs partition { _.iv == iv }
      same :: chunkDefaults(rest)
  }
}