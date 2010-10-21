package gsd.linux

object TExpr {

  /**
   * Limited functionality, used to convert reverse dependency expression.
   */
  def toTExpr(in: KExpr): TExpr = {
    def t(e: KExpr): TExpr = e match {
      case Id(n) => TId(n)

      case No  => TNo
      case Mod => TMod
      case Yes => TYes

      case And(x, y) => t(x) & t(y)
      case Or(x, y) => t(x) | t(y)
      case Eq(l, r) => t(l) teq t(r)
      case NEq(l, r) => !(t(l) teq t(r))
      case Not(e) => !t(e)

      case Literal(_) | KHex(_) | KInt(_) => error("Not handled yet!!!")

      case e => error("Unexpected expression (is it a boolean op?): " + e + ": " + e.getClass)
    }
    t(in)
  }

}

abstract class TExpr {
  def toBExpr: (BExpr, BExpr)

  /**
   * Returns a single boolean expression.
   */
  def eq(other: TExpr): BExpr = other match {
    case TNo => toBExpr match {
      case (e1, e2) => !e1 & !e2
    }
    case TYes => toBExpr match {
      case (e1, e2) => e1 & e2
    }

    case _ => (toBExpr, other.toBExpr) match {
      case ((l1, l2), (r1, r2)) => (l1 iff r1) & (l2 iff r2)
    }
  }

  /**
   * Returns a single boolean expression.
   */
  def <=(other: TExpr): BExpr = other match {
    case TYes => (this eq TYes) | (this eq TMod) | (this eq TNo)
    case TMod => (this eq TMod) | (this eq TNo)
    case TNo =>  (this eq TNo)

    case _ =>
      (toBExpr, other.toBExpr) match {
        case ((l1, l2), (r1, r2)) => ((l1 | l2 ) implies (r1 | r2)) & (l1 & l2 & r1 & !r2)
      }
  }

  /**
   * Returns a single boolean expression.
   */
  def >(other: TExpr): BExpr =
    !(this <= other)

  def teq(other: TExpr): TExpr =
    TEq(this, other)

  def &(other: TExpr): TExpr = TAnd(this, other)
  def |(other: TExpr): TExpr = TOr(this, other)
  def unary_! = TNot(this)
}

case object TYes extends TExpr {
  def toBExpr = (BTrue, BTrue) //(1,1)
}
case object TMod extends TExpr {
  def toBExpr = (BTrue, BFalse) //(1,0)
}
case object TNo extends TExpr {
  def toBExpr = (BFalse, BFalse) //(0,0)
}

case class TId(v: String) extends TExpr {
  def toBExpr = (BId(v + "_1"), BId(v + "_2"))
}

case class TAnd(left: TExpr, right: TExpr) extends TExpr {
  def toBExpr = (left.toBExpr, right.toBExpr) match {
    case ((l1, l2), (r1, r2)) => (l1 & r1, l2 & r2)
  }
}

case class TOr(left: TExpr, right: TExpr) extends TExpr {
  def toBExpr = (left.toBExpr, right.toBExpr) match {
    case ((l1, l2), (r1, r2)) => (l1 | r1, l2 | r2)
  }
}

case class TNot(e: TExpr) extends TExpr {
  def toBExpr = e.toBExpr match {
    case (l, r) => (!r, !l)
  }
}

case class TEq(left: TExpr, right: TExpr) extends TExpr {
  def toBExpr = (left.toBExpr, right.toBExpr) match {
    case ((l1, l2), (r1, r2)) => (l1 iff r1, l2 iff r2)
  }
}




class TFMTranslation {
  import TExpr._

  object IdGen {
    var i = 0
    def next = { i+=1; "_X" + i }
    def allIds = (1 to i).map { "_X" + _ }.toList
  }

  /**
   * FIXME Assuming: tristate, no inherited, ranges.
   * Always introducing new variable for reverse dependency expression.
   */
  def translate(c: AConfig): List[BExpr] = c match {
    case AConfig(id, t, inh, pro, defs, rev, ranges) =>
      val rds = ((TNo: TExpr)/: rev){ _ | toTExpr(_) }
      val rdsId = if (rds == TNo) TNo else TId(IdGen.next)

      println("Reverse Dependency: " + rds)
      def t(rest: List[Default], prev: List[Default]): List[BExpr] =
        rest match {
          case Nil => Nil
          
          case (h@Default(e,cond))::tail =>
            val ante = ((toTExpr(pro) eq TNo) /: prev){ (x,y) =>
              x & (toTExpr(y.cond) eq TNo)
            } & (toTExpr(cond) > TNo)

            //problem is here: toTExpr(e)
            (ante implies (TId(id) eq (toTExpr(e) | rdsId))) :: t(tail, h::prev)
        }

    (rdsId eq rds) :: t(defs, Nil)
  }

  def translate(k: AbstractKConfig): List[BExpr] =
    k.configs flatMap translate

}
