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

  implicit def toConfigList(lst: List[List[(String,String)]]) =
    new RichConfigList(lst)

  class RichConfigList(lst: List[List[(String,String)]]) {
    def filterConfigs(cond: ((String,String)) => Boolean): List[List[(String,String)]] =
      lst filter { l => l exists cond }

    def configValues(ids: String*): Set[String] =
      Set() ++ (lst flatMap { _ filter { case (c,_) => ids contains c } map { _._2 } })
  }


  @Test def boolean {
    val in = """
      config A1 boolean {
        prompt "A" if []
      }
      """

    assert(allConfigs(in).size == 2)
  }

  @Test def tristate {
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
  @Test def oneSelect {
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

  @Test def multipleSelectsDerived {
    val in = """
      config A1 tristate
      config B1 tristate { prompt "B" if [] select A1 if [] }
      config C1 tristate { prompt "C" if [] select A1 if [] }
      """

    allConfigs(in) foreach println

    // If A is M or Y, then either B1 or C1 is M or Y
    assert {
      allConfigs(in) filterConfigs
            { t => t == ("A1", "M") || t == ("A1", "Y") } forall
            { c => c exists { t => t == ("B1", "M") ||
                    t == ("C1", "M") ||
                    t == ("B1", "Y") ||
                    t == ("C1", "Y") }}
    }
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
        allConfigs(in) filterConfigs { t =>
          t == ("A1", "M") || t == ("A1", "Y")
      } configValues("B1") forall { v => v == "M" || v == "Y" }
    }
  }

  @Test def conditionalDerived {
    val in = """
      config A1 tristate {
        prompt "A" if [B1]
        default [m] if []
      }
      config B1 tristate { prompt "B" if [] }
      """

    allConfigs(in) foreach println

    //When B1 is N, A1 must be M
    expect(Set("M")) {
      allConfigs(in) filterConfigs { _ == ("B1", "N") } configValues("A1")
    }
    
    expect(7)(allConfigs(in).size)
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