package gsd.linux.tools

import collection.mutable.ListBuffer

import gsd.linux.TypeFilterList._

trait ArgBuilder {
  this: OptParser =>

  protected sealed abstract class ArgExp {
    def ||(other: ArgExp) = ArgOr(this, other)
    def &&(other: ArgExp) = ArgAnd(this, other)
    def unary_! = ArgNot(this)

    def evaluate(cm: ConfigMap): Boolean
  }
  protected case class ArgId(name: String) extends ArgExp {
    def evaluate(cm: ConfigMap): Boolean = cm.map.contains(name)
  }
  protected case class ArgOr(left: ArgExp, right: ArgExp) extends ArgExp {
    def evaluate(cm: ConfigMap): Boolean = left.evaluate(cm) || right.evaluate(cm)
  }
  protected case class ArgAnd(left: ArgExp, right: ArgExp) extends ArgExp {
    def evaluate(cm: ConfigMap): Boolean = left.evaluate(cm) && right.evaluate(cm)
  }
  protected case class ArgNot(e: ArgExp) extends ArgExp {
    def evaluate(cm: ConfigMap): Boolean = !e.evaluate(cm)
  }

  protected sealed abstract class Arg(val name: String)
  
  protected case class OptArg(n: String, default: Option[String]) extends Arg(n)
  protected case class MandArg(n: String) extends Arg(n)

  implicit def toArgId(s: String) = ArgId(s)

  def opt(name: String, default: Option[String] = None) =
    args += OptArg(name, default)

  def opt(name: String, default: String) =
    args += OptArg(name, Some(default))

  def mand(name: String) =
    args += MandArg(name)

  def constraint(e: ArgExp) =
    constraints += e
}

class ConfigMap (val map: Map[String, String],
                 val rest: List[String] = Nil) {

  def this(in: List[(String, String)]) = this(in.toMap)

  /** Scala 2.8 Implicit Parameters / Context Bounds. Implicitly find a conversion
   * and call the method convert(in) on it.
   *
   * @see http://stackoverflow.com/questions/3855595/scala-function-implicitly
   */
  def getAs[T: Conversion](s: String): Option[T] = map.get(s) match {
    case Some(x) => Some(implicitly[Conversion[T]].convert(x))
    case None => None
  }

  def definedKeys = map.keySet

  def ++(other: ConfigMap) = new ConfigMap(map ++ other.map, rest ::: other.rest)
}

class OptParser extends ArgBuilder {
  val args = new ListBuffer[Arg]
  val constraints = new ListBuffer[ArgExp]

  @throws(classOf[ParseException])
  def parse(in: Seq[String]): ConfigMap = {
    val al = args.toList

    val mandatory = al.typeFilter[MandArg]
    val optional  = al.typeFilter[OptArg]
    val argSet    = (args map { _.name }).toSet //for convenience

    def _p(lst: List[String], results: List[(String, String)]): ConfigMap =
      lst match {
        case Nil => new ConfigMap(results.toMap)

        case o :: rest if o(0) != '-' || o(1) != '-' =>
          new ConfigMap(results.toMap, lst)

        case o :: v :: rest if (argSet contains o.substring(2)) =>
            _p(rest, (o.substring(2), v) :: results)
        
        case x => throw UnknownArgsException(x)
      }

    val m = _p(in.toList, Nil)

    val notDefinedMandatory = mandatory filterNot { m.definedKeys contains _.name }
    val notDefinedOptional = optional filter
          { _.default.isDefined } filterNot //only those with defaults
          { m.definedKeys contains _.name } //only those that aren't defined in args

    if (notDefinedMandatory.size > 0)
      throw MissingMandArgsException(notDefinedMandatory map { _.name })

    val result = m ++ (new ConfigMap(notDefinedOptional map { c => c.name -> c.default.get }))

    val notSatisfied = constraints filterNot { _.evaluate(result) }
    if (notSatisfied.size > 0)
      throw ConstraintsException(notSatisfied.toList)
    result
  }

  case class MissingMandArgsException(args: List[String])
          extends ParseException("Missing arguments: %s".format(args.mkString(" ")))
  case class UnknownArgsException(args: List[String])
          extends ParseException("Unknown arguments: %s".format(args.mkString(" ")))
  case class ConstraintsException(constraints: List[ArgExp])
          extends ParseException("Unsatisfied Constraints: %s".format(constraints.mkString(" ")))
  sealed abstract class ParseException(msg: String)
          extends Exception(msg)
}

trait Conversion[T] {
  def convert(in: String): T
}

/** Implicit objects that are found by the implicitly function */
object Conversion {
  implicit object StringConversion extends Conversion[String] {
    def convert(in: String) = in
  }
  implicit object IntConversion extends Conversion[Int] {
    def convert(in: String) = in.toInt
  }
  implicit object DoubleConversion extends Conversion[Double] {
    def convert(in: String) = in.toDouble
  }
  implicit object BooleanConversion extends Conversion[Boolean] {
    def convert(in: String) = in.toBoolean
  }
}