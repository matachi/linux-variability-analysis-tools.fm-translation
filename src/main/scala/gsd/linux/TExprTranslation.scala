package gsd.linux

/**
 * Input: AbstractKConfig
 */
object TExprTranslation {

  def mkDefaults(c: AConfig): List[TExpr] = {

    def _mkExprs(rest: List[Default], prev: List[Default]): List[TExpr] = {
      val negatedPrev = ((TYes: TExpr) /: prev) { (x, y) => x & !TBool(y.cond) }

      rest match {
        case Nil => Nil
        case (head@Default(e,cond))::tail =>
          (TBool(c.pro) | (negatedPrev & TBool(cond) & (TId(c.id) eqs e))) ::
            _mkExprs(tail, head :: prev)
      }

    }

    _mkExprs(c.defs, Nil)
  }

  import TExpr._
  def mkBounds(c: AConfig): TExpr = 
    (TId(c.id) >= (c.rev.toTExprs.|| & (TId(c.id) <= c.pro)))

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