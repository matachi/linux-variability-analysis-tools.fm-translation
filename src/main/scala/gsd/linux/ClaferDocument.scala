package gsd.linux

import Document._


//FIXME hard-coded for boolean format
trait ClaferDocument extends B2ExprDocument {

  // Regex to match any character that isn't alphanumeric
  val pat = """[^a-zA-Z0-9]""".r

  // Clafer fix identifiers
  // def fix(s: String) = pat.replaceAllIn(s, "")

  def fix(s: String) =
    if (s.contains(" ")) "\"" + s + "\""
    else s


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
    case b: B2Expr => toExprText(b)
  }

  implicit def toText[T <: Expr](fm: FM[T]): Text =
    (fm.features map toText[T]) reduceLeft { _ :/: _ }

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


