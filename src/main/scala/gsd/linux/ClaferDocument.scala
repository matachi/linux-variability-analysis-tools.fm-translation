package gsd.linux

import Document._


//FIXME hard-coded for boolean format
trait ClaferDocument extends TExprDocument with B2ExprDocument {

  // Regex to match any character that isn't alphanumeric
  val pat = """[^a-zA-Z0-9]""".r

  // Clafer fix identifiers
  // def fix(s: String) = pat.replaceAllIn(s, "")

  // Do nothing
  def fix(s: String) = s


  // FIXME cross-tree constraint nesting
  def crossTree[T <: Expr](ctcs: List[T]) =
    if (ctcs.isEmpty) NL
    else NL +: Block("[", "]", iterToText(ctcs map toText[T])(_ :/: _)) +: NL

  def toText[T <: Expr](f: Node[T]): Text = f match {
    case OFeature(name,t,ctcs,cs) =>
      fix(name) +: "?" ::  Block("{", "}", cs map toText[T]) :: crossTree(ctcs)

    case MFeature(name,t,ctcs,cs) =>
      fix(name) :: Block("{", "}", cs map toText[T]) :: crossTree(ctcs)

    case OptGroup(name, cs, ctcs) =>
      "opt" :: fix(name) :: Block("{", "}", cs map toText[T]) :: crossTree(ctcs)

    case OrGroup(name, cs, ctcs) =>
      "or" :: fix(name) :: Block("{", "}", cs map toText[T]) :: crossTree(ctcs)

    case XorGroup(name, cs, ctcs) =>
      "xor" :: fix(name) :: Block("{", "}", cs map toText[T]) :: crossTree(ctcs)

    case MutexGroup(name, cs, ctcs) =>
      "mux" :: fix(name) :: Block("{", "}", cs map toText[T]) :: crossTree(ctcs)
  }

  def toText[T <: Expr](e: T): Text = e match {
    case t: TExpr => toExprText(t)
    case b: B2Expr => toExprText(b)
  }

  implicit def toText[T <: Expr](fm: FM[T]): Text =
    (fm.features map toText[T]) reduceLeft { _ :/: _ }

}

trait TExprDocument {

  def toExprText(e: TExpr): Text = {
    def _paren(e: TExpr): Text =
      if (e.isTerminal || e.getClass == this.getClass) toExprText(e) //FIXME
      else "(" +: toExprText(e) +: ")"

    e match {
      case TYes => "y"
      case TMod => "m"
      case TNo => "n"
      case TAnd(x,y) => _paren(x) :: "&" :: _paren(y)
      case TOr(x,y) => _paren(x) :: "|" :: _paren(y)
      case TEq(x,y) => _paren(x) :: "=" :: _paren(y)
      case TGte(x,y) => _paren(x) :: ">=" :: _paren(y)
      case TLte(x,y) => _paren(x) :: "<=" :: _paren(y)

      case TNot(x) => "!" +: toExprText(x)
      case TBool(x) => "Bool(" +: toExprText(x) +: string(")")
      case TId(x) => string(x)

      case TImplies(x,y) => _paren(x) :: "->" :: _paren(y)

      case _ => StringT(e.toString)
    }
  }

}

trait B2ExprDocument {

  def toExprText(e: B2Expr): Text = {
    def _paren(e: B2Expr): Text =
      if (e.isTerminal || e.getClass == this.getClass) toExprText(e) //FIXME
      else "(" +: toExprText(e) +: ")"

    e match {
      case B2True => "1"
      case B2False => "0"
      case B2And(x,y) => _paren(x) :: "&&" :: _paren(y)
      case B2Or(x,y) => _paren(x) :: "||" :: _paren(y)
      case B2Implies(x,y) => _paren(x) :: "=>" :: _paren(y)
      case B2Iff(x,y) => _paren(x) :: "<=>" :: _paren(y)

      case B2Not(x) => "~" +: toExprText(x)
      case B2Id(x) => string(x)

      case _ => StringT(e.toString)
    }
  }

}


