package gsd.linux

import collection.mutable.ListBuffer

/**
 * @param k
 * @param env A list of environment variables. These variables are ignored when
 *            creating constraints during the translation.
 * @param addUndefined Add constraints that cause undefined / undeclared configs
 *                     dead.
 */
class TristateTranslation(val k: AbstractKConfig,
                          val addUndefined: Boolean = true) {
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
    ((k.configs flatMap translate) ::: (k.choices flatMap translate)) map
      (_.simplify) filter (_ != BTrue)


  def translate(c: AChoice): List[BExpr] = c match {
    // Boolean, mandatory choice == XOR group
    case AChoice(vis, true, isOpt, members) =>
      // exclusions between all members
      val exclusions = Combinations.choose(2, members) map { 
        case List(x,y) => !BId(x) | !BId(y)
        case _ => sys.error("This should never occur since we use Combinations.choose(2,...)")
      }

      // Visibility condition should be passed down to choice members
      
     val mandImpl =
       if (isOpt) Some((toTExpr(vis) > TNo) implies members.map(BId(_): BExpr).reduce(_|_))
       else None

     mandImpl.toList ::: exclusions

    // any other kind of group doesn't impose a constraint
    case _ => Nil
  }

  /**
   * Always introducing new variable for reverse dependency expression.
   */
  def translate(c: AConfig): List[BExpr] = c match {

    // TODO verify that environment variables shouldn't impose any constraint
    case AConfig(_,name, _, _, _, _, _, _) if k.env contains name =>
      Nil

    case AConfig(nodeId, name, t, inh, pro, defs, rev, ranges) =>

      // use a generated variable for each expression in the reverse dependency
      val rds = rev map { r => (toTExpr(r), TId(IdGen.next)) }

      val rdsExpr = ((TNo: TExpr) /: rds){ case (x,(_, id)) => x | id }

      // rdsEquiv will be Some if a generated variable is used to represent
      // entire reverse dependency expression
      val (rdsId, rdsEquiv): (TId, BExpr) = {
          val id = IdGen.next
          (TId(id), rdsExpr beq TId(id))
        }

      // create default constraints
      def defaults(rest: List[ADefault]): List[BExpr] = {

        // uses a BId to represent the negated previous conditions
        def t(rest: List[ADefault], prevCondId: BId): List[BExpr] = rest match {
          case Nil => Nil

            // FIXME not using prev on ADefault
          case ADefault(e,_,cond)::tail =>

            // generate condition id for next iteration of t(..)
            //TODO optimization: not necessary if tail is empty
            val (nextCondId, nextCondEquiv) = {
              val id = IdGen.next
              val (bid1, bid2) = (BId(id), BId(id + "_m"))

              //FIXME id generator still generates _2 variable, so we make it dead
              (bid1, (bid1 iff (prevCondId & (toTExpr(cond) beq TNo))) & !bid2)
            }

            // antecedent for current default
            val ante = prevCondId & (toTExpr(cond) > TNo)

            // Handle default y quirk (i.e. if default y, then config takes
            // value of its condition, not y)
            val tex = if (e == Yes) toTExpr(cond) else toTExpr(e)

            // tex | rdsId: config takes the max of the default or the RDs
            (ante implies (TId(name) beq (tex | rdsId))) ::
              nextCondEquiv :: // default condition equivalence
              t(tail, nextCondId)
        }

        // negated prompt condition for defaults (since prevCondId represents
        // the negated previous condition)
        val (proId, proEquiv) = {
          val id = IdGen.next
          val (bid1, bid2) = (BId(id), BId(id + "_m"))

           //FIXME id generator still generates _2 variable, so we make it dead
          (bid1, (bid1 iff (toTExpr(pro) beq TNo)) & !bid2)
        }

        // The prompt acts as the first negated condition
        proEquiv :: t(rest, proId)
      }

      // Disallow mod state from Boolean, String, Int and Hex configs
      val typeConstraint: Option[BExpr] = t match {
        case KBoolType | KStringType | KIntType | KHexType =>
          Some(BId(name) implies BId(name + "_m") )
        case _ => None
      }

      // Disallow (0,1) state
      val tristateConstraints: List[BExpr] =
        AbstractKConfig.identifiers(c).toList map { id => BId(id) | !BId(id + "_m") }

      // Make undefined variables dead referenced in this config
      val undefinedConstraints: List[BExpr] =
        if (addUndefined) ((AbstractKConfig.identifiers(c) -- (k.configs map (_.name)) -- k.env) map (!BId(_))).toList
        else Nil
      
      // an upper bound is only imposed on tristate configs, if it's parent is also tristate
      val upperBoundConstraints: Option[BExpr] = (t, k.parentMap.get(c)) match {
        case (KTriType, Some(AConfig(_,parentName,KTriType,_,_,_,_,_))) => Some(TId(name) <= TId(parentName))
        case _ => None
      }

      (rds map { case (e, id) => id beq e }) ::: // reverse dependency sub-expressions
        rdsEquiv :: // reverse dependency equivalence
        (rdsId <= TId(name)) ::  // reverse dependency lower bound
        typeConstraint.toList :::
        // upperBoundConstraints.toList ::: FIXME ignoring upper bound
        tristateConstraints :::
        undefinedConstraints :::
        defaults(defs) // defaults
  }

}

object TristateTranslation {
  trait ConsoleHelper {
    this: TristateTranslation =>

    def translate(configName: String): List[BExpr] =
      k.findConfig(configName) match {
        case Some(config) => translate(config)
        case None => sys.error("Config %s not found".format(configName))
      }
  }

  // Convenience function for translating from console
  def apply(file: String) =
    new TristateTranslation(KConfigParser.parseKConfigFile(file)) with ConsoleHelper
  
}
