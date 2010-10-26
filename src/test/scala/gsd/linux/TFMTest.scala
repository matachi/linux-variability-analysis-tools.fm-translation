package gsd.linux

import cnf.SATBuilder
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

class TFMTest extends AssertionsForJUnit {

  import KConfigParser._

  def allConfigs(in: String): List[List[(String,String)]] = {
    val ak = parseKConfig(in).toAbstractKConfig
    val trans = new TFMTranslation(ak)
    val exprs = trans.translate
    val sat = new SATBuilder(exprs, trans.idMap)
    sat.allConfigurations map trans.interpret
  }

  implicit def toConfigList(lst: List[List[(String,String)]]) = new {
    def filterByConfig(cond: ((String,String)) => Boolean): List[List[(String,String)]] =
      lst filter { l => l exists cond }

    def configValue(id: String): List[String] =
      lst map { l => l.find { case (c,_) => c == id }.get._2 }
  }

  @Test def single {
    val in = """
      config A tristate {
        prompt "..." if []
      }"""

    assert(allConfigs(in).size === 3)
  }


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
      config W1 tristate { prompt "..." if [] }
      config X1 tristate { prompt "..." if [] }
      config Y1 tristate { prompt "..." if [] }
      config Z1 tristate { prompt "..." if [] }
      """
    allConfigs(in) foreach println
  }

  /**
   * A1 should be lower bounded by B1.
   */
  @Test def select {
    val in = """
      config A1 tristate {
        prompt "A" if []
      }
      config B1 tristate {
        prompt "B" if []
        select A1 if []
      }
      """

    assert(List(
        List(("A1", "N"), ("B1", "N")),
        List(("A1", "M"), ("B1", "N")),
        List(("A1", "Y"), ("B1", "N")),
        List(("A1", "M"), ("B1", "M")),
        List(("A1", "Y"), ("B1", "M")),
        List(("A1", "Y"), ("B1", "Y"))) === allConfigs(in))
  }

  @Test def defaultY {
    val in = """
      config A1 tristate {
        default [y] if [B1]
      }
      config B1 tristate { prompt "B" if [] }
      """

    assert(List(
        List(("A1","N"), ("B1", "N")),
        List(("A1","M"), ("B1", "M")),
        List(("A1","Y"), ("B1", "Y"))) === allConfigs(in))
  }

  @Test def defaultM {
    val in = """
      config A1 tristate {
        default [m] if [B1]
      }
      config B1 tristate { prompt "B" if [] }
      """

    assert(List(
      List(("A1","N"), ("B1", "N")),
      List(("A1","M"), ("B1", "M")),
      List(("A1","M"), ("B1", "Y"))) === allConfigs(in))
  }

  @Test def defaultWithPrompt {
    val in = """
      config A1 tristate {
        prompt "A" if []
        default [y] if [B1]
      }
      config B1 tristate { prompt "B" if [] }
      """

    assert(allConfigs(in).size === 9)
  }

  @Test def conditionalPrompt {
    val in = """
      config A1 tristate {
        prompt "A" if [B1]
      }
      config B1 tristate { prompt "B" if [] }
      """

    // A1 can only be M or Y when B1 is M or Y
    assert {
        allConfigs(in) filterByConfig { t =>
          t == ("A1", "M") || t == ("A1", "Y")
      } configValue("B1") forall { v => v == "M" || v == "Y" }
    }
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

  @Test def simplify {
    implicit def toBExpr(id: String) = BId(id)
    assert((BId("B") | BFalse).simplify === BId("B"))
    assert(("A" iff (BId("B") | BFalse)).simplify === ("A" iff "B"))
  }

}