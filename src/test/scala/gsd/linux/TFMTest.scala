package gsd.linux

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

class TFMTest extends AssertionsForJUnit {

  import KConfigParser._

  @Test def everything {
    val in = """
      config A tristate {
        prompt "A" if [P]
        default [y] if [W]
        default [m] if [X]
      }
      config B tristate {
        prompt "B" if []
        select A if [Y]
      }
      config C tristate {
        prompt "C" if []
        select A if [Z]
      }
      config P tristate
      config W tristate
      config X tristate
      config Y tristate
      config Z tristate
      """

    val ak = parseKConfig(in).toAbstractKConfig
    TFMTranslation.translate(ak) foreach println
  }

}