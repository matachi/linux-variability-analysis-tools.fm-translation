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
  def toBExpr: (B2Expr, B2Expr)

  /**
   * Returns a single boolean expression.
   */
  def eq(other: TExpr): B2Expr = other match {
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
  def <=(other: TExpr): B2Expr = other match {
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
  def >(other: TExpr): B2Expr =
    !(this <= other)

  def teq(other: TExpr): TExpr =
    TEq(this, other)

  def &(other: TExpr): TExpr = TAnd(this, other)
  def |(other: TExpr): TExpr = TOr(this, other)
  def unary_! = TNot(this)
}

case object TYes extends TExpr {
  def toBExpr = (B2True, B2True) //(1,1)
}
case object TMod extends TExpr {
  def toBExpr = (B2True, B2False) //(1,0)
}
case object TNo extends TExpr {
  def toBExpr = (B2False, B2False) //(0,0)
}

case class TId(v: String) extends TExpr {
  def toBExpr = (B2Id(v + "_1"), B2Id(v + "_2"))
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




object TFMTranslation {

  import TExpr._

  /**
   * FIXME Assuming: tristate, no inherited, ranges.
   * Always introducing new variable for reverse dependency expression.
   */
  def translate(c: AConfig): List[B2Expr] = c match {
    case AConfig(id, t, inh, pro, defs, rev, ranges) =>
      val rds = ((TNo: TExpr)/: rev){ _ | toTExpr(_) }

      println("Reverse Dependency: " + rds)

      def t(rest: List[Default], prev: List[Default]): List[B2Expr] =
        rest match {
          case Nil => Nil
          
          case (h@Default(e,cond))::tail =>
            val ante = ((toTExpr(pro) eq TNo) /: prev){ (x,y) =>
              x & (toTExpr(y.cond) eq TNo)
            } & (toTExpr(cond) > TNo)

            //problem is here: toTExpr(e)
            (ante implies (TId(id) eq (toTExpr(e) | rds))) :: t(tail, h::prev)
        }

    t(defs, Nil)
  }

  def translate(k: AbstractKConfig): List[B2Expr] =
    k.configs flatMap translate

}
