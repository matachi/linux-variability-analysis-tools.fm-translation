package gsd.linux

import kiama.rewriting.Rewriter

object ExprUtil extends Rewriter {


  def removeTrueAndFalse[T <: Expr](fm: FM[T]): FM[T] = {
    val f1 = rewrite {
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
        }
      }
    }(fm)

    rewrite {
      outermost {
          rule {
            case lst: List[_] if lst.contains(B2True) =>
              lst remove { _ == B2True }
          }
      }
    }(f1)
  }
}