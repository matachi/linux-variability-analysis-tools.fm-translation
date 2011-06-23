package gsd.linux

import collection.mutable.ListBuffer
import annotation.tailrec

class TristateTranslation(k: AbstractKConfig) {
  import TExpr._

  object IdGen {
    val prefix = "_X"
    var i = 0
    def next = { i+=1; prefix + i }
    def allIds = (1 to i).map { prefix + _ }.toList
  }

  def generated: List[String] =
    IdGen.allIds ::: (IdGen.allIds map { _ + "_m"})

  def identifiers: List[String] =
    k.identifiers.toList ::: IdGen.allIds

  def size: Int = identifiers.size

  /*
   * Var i (odd) represents identifier x, Var i+1 (even) represents x_m
   */
  def idMap: Map[String, Int] =
    Map() ++ {
      (identifiers flatMap
              { id => List(id, id + "_m") }).zipWithIndex map
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
      val id = varMap(key)
      val state = (i1, i2) match {
        case (true, true)   => 2
        case (true, false)  => 1
        case (false, false) => 0
        case (false, true)  => sys.error("(0,1) state should never be in a model!")
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
    ((k.configs flatMap translate) filter{ _ != BTrue }) ::: {
      // Disallow mod state from Boolean configs

      k.configs filter
              { _.ktype == KBoolType } map
              { _.id } map
              { id => BId(id) implies BId(id + "_m") }
    } ::: {
      // Disallow mod state from String, Int and Hex configs

      k.configs filter
              { t => t.ktype == KIntType || t.ktype == KHexType || t.ktype == KStringType } map
              { _.id } map
              { id => BId(id) implies BId(id + "_m") }
    } ::: {
      // Disallow (0,1) state
      // Ignore generated identifiers because they have equivalence

      k.identifiers.toList map { id => BId(id) | !BId(id + "_m") }
    }

  /**
   * FIXME no ranges.
   * Always introducing new variable for reverse dependency expression.
   */
  def translate(c: AConfig): List[BExpr] = c match {
    case AConfig(id, t, inh, pro, defs, rev, ranges) =>

      // use a generated variable for each expression in the reverse dependency
      val rds = rev map { r => (toTExpr(r), TId(IdGen.next)) }

      val rdsExpr = ((TNo: TExpr) /: rds){ case (x,(_, id)) => x | id }

      // rdsEquiv will be Some if a generated variable is used to represent
      // entire reverse dependency expression
      val (rdsId, rdsEquiv): (TExpr, Option[BExpr]) =
        if (rds.size < 4) (rdsExpr, None)
        else {
          val id = TId(IdGen.next)
          (id, Some(rdsExpr eq id))
        }

      // create default constraints
      def defaults(rest: List[Default]): List[BExpr] = {

        // uses a BId to represent the negated previous conditions
        def t(rest: List[Default], prevCondId: BId): List[BExpr] = rest match {
          case Nil => Nil

          case (h@Default(e,cond))::tail =>

            // generate condition id for next iteration of t(..)
            //TODO optimization: not necessary if tail is empty
            val (nextCondId, nextCondEquiv) = {
              val id = IdGen.next
              val (bid1, bid2) = (BId(id), BId(id + "_m"))

              //FIXME id generator still generates _2 variable, so we make it dead
              (bid1, (bid1 iff (prevCondId & (toTExpr(cond) eq TNo))) & !bid2)
            }

            // antecedent for current default
            val ante = prevCondId & (toTExpr(cond) > TNo)

            // Handle default y quirk (i.e. if default y, then config takes
            // value of its condition, not y)
            val tex = if (e == Yes) toTExpr(cond) else toTExpr(e)

            // tex | rdsId: config takes the max of the default of the RDs
            (ante implies (TId(id) eq (tex | rdsId))) ::
              nextCondEquiv :: // default condition equivalence
              t(tail, nextCondId)
        }

        // negated prompt condition for defaults (since prevCondId represents
        // the negated previous condition)
        val (proId, proEquiv) = {
          val id = IdGen.next
          val (bid1, bid2) = (BId(id), BId(id + "_m"))

           //FIXME id generator still generates _2 variable, so we make it dead
          (bid1, (bid1 iff (toTExpr(pro) eq TNo)) & !bid2)
        }

        proEquiv :: t(rest, proId)
      }

    (rds map { case (e, id) => id eq e }) ::: // reverse dependency sub-expressions
            rdsEquiv.toList ::: // reverse dependency equivalence
            (rdsId <= TId(id)) ::  // reverse dependency lower bound
            (TId(id) <= toTExpr(inh)) ::  // inherited upper bound
            defaults(defs) // defaults
  }

}