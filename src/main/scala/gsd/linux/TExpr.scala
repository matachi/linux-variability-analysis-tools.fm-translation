package gsd.linux

import collection.mutable.ListBuffer
import util.logging.Logged

object TExpr extends Logged {

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

      case Literal("") => //FIXME ?
        TNo
      case Literal(_) | KHex(_) | KInt(_) =>
        log("WARN: Literal / Hex / Int not handled, returning TYes")
        TYes

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

    case TMod => toBExpr match {
      case (e1, e2) => e1 & !e2
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
  def <=(other: TExpr): BExpr = (this, other) match {
    
    case (TNo,_)  => BTrue
    case (TYes,_) => other eq TYes

    case (_,TYes) => BTrue
    case (_,TNo)  => this eq TNo

    case _ => (toBExpr, other.toBExpr) match {

        // x <= Mod
        case ((l1, l2), (BTrue,BFalse)) => !l2

        //Mod <= x
        case ((BTrue, BFalse),(r1, r2)) => r1

        case ((l1, l2), (r1, r2)) =>
          // Before simplifying:
          // (!(l1 | l2 ) implies (r1 | r2)) & !(l1 & l2 & r1 & !r2)
          (!l1 | r1 | r2) & (!l2 | r1 | r2 ) & (!l1 | !l2 | !r1 | r2)
      }
  }

  /**
   * Returns a single boolean expression.
   */
  def >(other: TExpr): BExpr = (this, other) match {
    
    case (TMod,_) => this eq TNo
    case (_,TMod) => this eq TYes

    case _ => (toBExpr, other.toBExpr) match {
        //Yes > x
        case ((BTrue, BTrue), (r1, r2)) => !r2

        //x > No
        case ((l1, l2), (BFalse, BFalse)) => l1

        //FIXME
        case ((l1, l2), (r1, r2)) =>
          l1 & !r2 & (!l1 | l2 | !r1 | r2)
      }
  }

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



class TFMTranslation(k: AbstractKConfig) {
  import TExpr._

  object IdGen {
    val prefix = "_X"
    var i = 0
    def next = { i+=1; prefix + i }
    def allIds = (1 to i).map { prefix + _ }.toList
  }

  def generated: Set[String] =
    Set() ++ IdGen.allIds

  def identifiers: Set[String] =
    Set() ++ k.identifiers ++ IdGen.allIds
  
  def size: Int = identifiers.size

  /*
   * Var i (odd) represents identifier x_1, Var i+1 (even) represents x_2.
   */
  def idMap: Map[String, Int] =
    Map() ++ {
      (identifiers.toList flatMap
              { id => List(id + "_1", id + "_2") }).zipWithIndex map
                     { case (id,i) => (id, i + 1) }
    }


  def varMap: Map[Int, String] =
    Map() ++ (idMap map { case (id,v) => (v,id) })


  def interpret(model: Array[Int]): List[(String, Int)] = {
    val varMap = Map() ++ (idMap map { case (id,v) => (v, id) })
    val result = new ListBuffer[(String, Int)]

    // Iterate only through identifiers in the Kconfig (ignoring generated)
    for (i <- 0 until model.length by 2) {
      val key = i + 1

      assert(math.abs(model(i)) == key, model(i) + " != " + key)
      val i1 = model(i) > 0
      val i2 = model(i+1) > 0
      val id: String = varMap(key).slice(0, varMap(key).length - 2) //FIXME drop _1 suffix
      val state = (i1, i2) match {
        case (true, true)   => 2
        case (true, false)  => 1
        case (false, false) => 0 
        case (false, true)  => error("(0,1) state should never be in a model!")
      }
      result += Tuple2(id, state)
    }
    
  result.toList
  }

  /**
   * Stateful: Changes identifiers in IdGen
   */
  lazy val translate: List[BExpr] =
    (translateNotSimplified map { _.simplify }) filterNot { _ == BTrue }

  lazy val translateNotSimplified: List[BExpr] =
    ((k.configs flatMap translate) filterNot { _ == BTrue}) ::: {
      // Disallow mod state from Boolean configs
      
      k.configs filter
              { _.ktype == KBoolType } map
              { _.id } map
              { id => BId(id + "_1") implies BId(id + "_2") }
    } ::: {
      // Disallow mod state from String, Int and Hex configs
      
      k.configs filter
              { t => t.ktype == KIntType || t.ktype == KHexType || t.ktype == KStringType } map
              { _.id } map
              { id => BId(id + "_1") implies BId(id + "_2") }
    } ::: {
      // Disallow (0,1) state
      // Ignore generated identifiers because they have equivalence
      
      k.identifiers.toList map { id => BId(id + "_1") | !BId(id + "_2") }
    }

  /**
   * FIXME no ranges.
   * Always introducing new variable for reverse dependency expression.
   */
  def translate(c: AConfig): List[BExpr] = c match {
    case AConfig(id, t, inh, pro, defs, rev, ranges) =>

      val rds = rev map { r => (toTExpr(r), TId(IdGen.next)) }
      //FIXME converting every reverse dependency to new id
      val rdsExpr = ((TNo: TExpr) /: rds){ case (x,(_, id)) => x | id }
      val rdsId = if (rds == TNo) TNo else TId(IdGen.next)

      def t(rest: List[Default], prev: List[Default]): List[BExpr] =
        rest match {
          case Nil => Nil

          case (h@Default(e,cond))::tail =>
            val ante = ((toTExpr(pro) eq TNo) /: prev){ (x,y) =>
              x & (toTExpr(y.cond) eq TNo)
            } & (toTExpr(cond) > TNo)

            // Handle default y quirk
            val tex = if (e == Yes) toTExpr(cond) else toTExpr(e)

            (ante implies (TId(id) eq (tex | rdsId))) :: t(tail, h::prev)
        }

    (rds map { case (e, id) => id eq e }) ::: // reverse dependency equivalence
    (rdsId eq rdsExpr) :: // reverse dependency equivalence
            (rdsId <= TId(id)) ::  // reverse dependency lower bound
            (TId(id) <= toTExpr(inh)) ::  // inherited upper bound
            t(defs, Nil)
  }

}
