package gsd.linux

import kiama.rewriting.Rewriter

object ExprUtil extends Rewriter {

  val sFixExpr =
    innermost {
      rule {
        case B2Or(B2True, y) => B2True
        case B2Or(x, B2True) => B2True
        case B2Or(B2False, y) => y
        case B2Or(x, B2False) => x
        case B2And(B2True, y) => y
        case B2And(x, B2True) => x
        case B2And(B2False, y) => B2False //TODO I hope this doesn't happen
        case B2And(x, B2False) => B2False //TODO I hope this doesn't happen
        case B2Implies(B2False, y) => B2True
        case B2Implies(B2True, y) => y
        case B2Implies(x,B2False) => !x
        case B2Implies(x,B2True) => B2True
        case B2Not(B2Not(x)) => x
      }
    }

  def removeTrue[T <: Expr](lst: List[T]): List[T] =
    lst remove { _ == B2True }

  def rewriteExpr[T <: Expr](lst: List[T]): List[T] =
    rewrite(sFixExpr)(lst)
  
}