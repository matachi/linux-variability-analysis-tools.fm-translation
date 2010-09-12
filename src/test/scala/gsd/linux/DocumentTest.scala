package gsd.linux

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

class DocumentTest extends AssertionsForJUnit with ClaferDocument {

  type T = TExpr

  @Test
  def simple {
    val fm = OFeature[T]("A", BoolFeat, Nil, List(
               OFeature[T]("B", BoolFeat, Nil, List(
                 OFeature[T]("X", BoolFeat, Nil, List(
                   OFeature[T]("Y", BoolFeat, Nil, Nil)
                   )),
                 OFeature[T]("Z", BoolFeat, Nil, Nil)
                 )),
               OFeature[T]("C", BoolFeat, Nil, Nil),
               OFeature[T]("D", BoolFeat, Nil, Nil)
             ))

    val text = toText(fm)
    println(text)
    text.format(System.out)
  }

}