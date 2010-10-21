package gsd.linux

import cnf.{IdMap, CNFBuilder}
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

class TFMTest extends AssertionsForJUnit {

  import KConfigParser._

  @Test def everything {
    val in = """
      config A1 tristate {
        prompt "A" if [P1]
        default [y] if [W1]
        default [m] if [X1]
      }
      config B1 tristate {
        prompt "B" if []
        select A1 if [Y1]
      }
      config C1 tristate {
        prompt "C" if []
        select A1 if [Z1]
      }
      config P1 tristate {
        prompt "P" if []
      }
      config W1 tristate
      config X1 tristate
      config Y1 tristate
      config Z1 tristate
      """

    val ak = parseKConfig(in).toAbstractKConfig
    val trans = new TFMTranslation
    val idMap = new IdMap
    ((trans.translate(ak) map { _.simplify }) - BTrue) foreach { e =>
      println(e)
      idMap += e
      println("CNF: " + CNFBuilder.toCNF(e, idMap))
    }
  }

}