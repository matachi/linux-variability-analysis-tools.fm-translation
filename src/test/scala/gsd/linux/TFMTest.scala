package gsd.linux

import cnf.SATBuilder
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

class TFMTest extends AssertionsForJUnit {

  import KConfigParser._

  def allConfigs(in: String): List[List[(String,Int)]] = {
    val ak = parseKConfig(in).toAbstractKConfig
    val trans = new TFMTranslation(ak)
    val exprs = trans.translate
    val sat = new SATBuilder(exprs, trans.idMap)
    sat.allConfigurations map trans.interpret
  }
  
  implicit def toRichConfig(lst: List[(String, Int)]) =
    new RichConfig(lst)

  class RichConfig(c: List[(String, Int)]) {
    def valueOf(s: String): Int =
      c.find { case (id,_) => id == s }.get._2
  }

  implicit def toRichAllConfig(lst: List[List[(String, Int)]]) =
    new RichAllConfig(lst)

  class RichAllConfig(lst: List[List[(String, Int)]]) {

    def filterConfigs(id: String)(f: Int => Boolean) =
      lst filter { _ exists { case (x,v) => x == id && f(v) } }

    def valuesOf(s: String): Set[Int] =
      Set() ++ (lst map { _ valueOf s })
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
        List(("A1", 0), ("B1", 0)),
        List(("A1", 1), ("B1", 0)),
        List(("A1", 2), ("B1", 0)),
        List(("A1", 1), ("B1", 1)),
        List(("A1", 2), ("B1", 1)),
        List(("A1", 2), ("B1", 2))) === allConfigs(in))
  }

  @Test def multipleSelectsDerived {
    val in = """
      config A1 tristate
      config B1 tristate { prompt "B" if [] select A1 if [] }
      config C1 tristate { prompt "C" if [] select A1 if [] }
      config D1 tristate { prompt "D" if [] select A1 if [] }
      """

    // If A1 is M or Y, then either B1, C1, or D1 is M or Y
    assert {
      allConfigs(in).filterConfigs("A1"){ _ >= 1 } forall
            { c => c exists { t =>
                    t == ("B1", 1) ||
                    t == ("C1", 1) ||
                    t == ("D1", 1) ||
                    t == ("B1", 2) ||
                    t == ("C1", 2) ||
                    t == ("D1", 2) }
            }
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
        List(("A1",0), ("B1", 0)),
        List(("A1",1), ("B1", 1)),
        List(("A1",2), ("B1", 2))) === allConfigs(in))
  }

  @Test def defaultM {
    val in = """
      config A1 tristate {
        default [m] if [B1]
      }
      config B1 tristate { prompt "B" if [] }
      """

    assert(List(
      List(("A1",0), ("B1", 0)),
      List(("A1",1), ("B1", 1)),
      List(("A1",1), ("B1", 2))) === allConfigs(in))
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
      allConfigs(in).filterConfigs ("A1") { _ >= 1 } forall
      { c =>  (c valueOf "B1") >= 1 }
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
    expect(Set(1)) {
      allConfigs(in).filterConfigs ("B1") { _ == 0 } valuesOf("A1")
    }
    
    expect(7)(allConfigs(in).size)
  }

  @Test def inherited {
    val in = """
      config A1 tristate {
        prompt "A1" if []
        config A2 tristate {
          prompt "A2" if []
          inherited [A1]
        }
      }
      """

    assert {
      allConfigs(in) forall { c =>
        (c valueOf "A1") <= (c valueOf "A2")
      }
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