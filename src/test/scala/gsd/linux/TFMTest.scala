package gsd.linux

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
      config P1 tristate
      config W1 tristate
      config X1 tristate
      config Y1 tristate
      config Z1 tristate
      """

    val ak = parseKConfig(in).toAbstractKConfig
    TFMTranslation.translate(ak) map { _.simplify } foreach println
  }

}