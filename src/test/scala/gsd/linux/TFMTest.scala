package gsd.linux

import cnf.{SATBuilder, IdMap, CNFBuilder}
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.sat4j.specs.ISolver
import org.sat4j.minisat.SolverFactory
import org.sat4j.tools.ModelIterator
import org.sat4j.core.VecInt
import java.util.Arrays

class TFMTest extends AssertionsForJUnit {

  import KConfigParser._

  @Test def everythingKconfig {
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
    val trans = new TFMTranslation(ak)
    val exprs = trans.translate

    val sat = new SATBuilder(exprs, trans.identifiers)
    println(sat.isSatisfiable)
    sat.allConfigurations map { Arrays.toString } foreach println
  }

  @Test def allConfigurations {
    {
      val sat = new SATBuilder(List(List(1,2,3), List(-1,-2,-3)), 3)
      assert(sat.allConfigurations.size === 6)
    }
    {
      val sat = new SATBuilder(List(List(1,-1)), 1)
      assert(sat.allConfigurations.size === 2)
    }
    {
      val sat = new SATBuilder(Nil, 1)
      assert(sat.allConfigurations.size === 2)
    }
  }

  @Test def allConfigurationsWithAssumptions {
    val sat = new SATBuilder(List(List(1,2,3), List(-1,-2,-3)), 3)
    assert(sat.allConfigurations(List(1)).size === 3)
    sat.reload
    assert(sat.allConfigurations(List(2)).size === 3)
    sat.reload
    assert(sat.allConfigurations(List(3)).size === 3)
  }

  @Test def allConfigurationsKconfig {
    val in = """
      config A tristate {
        prompt "..." if []
      }"""

    val ak = parseKConfig(in).toAbstractKConfig
    val trans = new TFMTranslation(ak)
    val sat = new SATBuilder(trans.translate, ak.idMap) //FIXME idMap
    assert(sat.allConfigurations.size == 2)
  }

}