package gsd.linux

object TExpr {
  implicit def toTExprList(lst: List[TExpr]): TExprList = new TExprList(lst)
  implicit def toKExprList(lst: List[KExpr]) = new {
    def toTExprs(): List[TExpr] = lst map toTExpr
  }

  class TExprList(lst: List[TExpr]){
    def ||(): TExpr = ((TNo: TExpr) /: lst){ _ | _ }
    def &&(): TExpr = ((TYes: TExpr) /: lst){ _ & _ }
  }

  implicit def toTExpr(e: KExpr): TExpr = e match {
    case And(l,r) => TAnd(l,r)
    case Or(l,r) => TOr(l,r)
    case Eq(l,r) => TEq(l,r)
    case NEq(l,r) => TNEq(l,r)
    case Not(e) => TNot(e)
    case Id(v) => TId(v)
    case Literal(v) => TLiteral(v)
    case KInt(v) => TInt(v)
    case KHex(v) => TInt(-99999) //FIXME
    case Yes => TYes
    case Mod => TMod
    case No => TNo
  }
}

/**
 * Extended KExpr with support for operators not defined in Kconfig.
 */
sealed abstract class TExpr {
  def &(o: TExpr): TExpr = TAnd(this, o)
  def |(o: TExpr): TExpr = TOr(this, o)
  def <(o: TExpr): TExpr = TLt(this, o)
  def >(o: TExpr): TExpr = TGt(this, o)
  def <=(o: TExpr): TExpr = TLte(this, o)
  def >=(o: TExpr): TExpr = TGte(this, o)
  def eqs(o: TExpr): TExpr = TEq(this, o)
  def unary_! = TNot(this)
}

case class TNot(e: TExpr) extends TExpr
case class TAnd(l: TExpr, r: TExpr) extends TExpr
case class TOr(l: TExpr, r: TExpr) extends TExpr
case class TEq(l: TExpr, r: TExpr) extends TExpr
case class TNEq(l: TExpr, r: TExpr) extends TExpr

case class TGt(l: TExpr, r: TExpr) extends TExpr
case class TGte(l: TExpr, r: TExpr) extends TExpr
case class TLt(l: TExpr, r: TExpr) extends TExpr
case class TLte(l: TExpr, r: TExpr) extends TExpr

case class TId(value: String) extends TExpr
case class TLiteral(value: String) extends TExpr
case class TInt(value: Int) extends TExpr
case object TYes extends TExpr
case object TMod extends TExpr
case object TNo extends TExpr

/**
 * Helper function for return TYes if v > 0, TNo otherwise.
 */
case class TBool(v: TExpr) extends TExpr

