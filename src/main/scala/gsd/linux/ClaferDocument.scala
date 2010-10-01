package gsd.linux

import Document._

trait ClaferTransforms {
  def fix(s: String): String =
    if (s == "no") "cNo" // reserved word
    else if (s.matches("""^[0-9].*""")) fix("c" + s)
    else s.replaceAll("""[^a-zA-Z0-9_]""", "_")
}

//FIXME hard-coded for boolean format
trait ClaferDocument extends B2ExprDocument {

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

trait B2ExprDocument extends ClaferTransforms {

  def toExprText(e: B2Expr): Text = {
    def _paren(e: B2Expr): Text =
      if (e.isTerminal || e.getClass == this.getClass) toExprText(e) //FIXME a better way of determining bracket nesting
      else "(" +: toExprText(e) +: ")"

    e match {
      case B2True => "1"  //FIXME Clafer doesn't have true
      case B2False => "0" //FIXME Clafer doesn't have false
      case B2And(x,y) => _paren(x) :: "&&" :: _paren(y)
      case B2Or(x,y) => _paren(x) :: "||" :: _paren(y)
      case B2Implies(x,y) => _paren(x) :: "=>" :: _paren(y)
      case B2Iff(x,y) => _paren(x) :: "<=>" :: _paren(y)

      case B2Not(x) => "~" +: toExprText(x)
      case B2Id(x) => string(fix(x))

      case _ => StringT(e.toString)
    }
  }

}


