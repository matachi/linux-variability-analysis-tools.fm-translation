package gsd.linux

import org.junit.Test

import cnf._

class BooleanTranslationTest {
  sealed abstract class Lit
  case class Pos(id: String) extends Lit {
    def unary_- = Neg(id)
  }
  case class Neg(id: String) extends Lit {
    def unary_- = Pos(id)
  }

  implicit def toLit(id: String) = Pos(id)

  import BooleanTranslation._

  def isKConfigSAT(ak: AbstractKConfig, assumps: Lit*): Boolean = {
    val trans = mkBooleanTranslation(ak)
    val idMap = ak.idMap ++
      (trans.genVars.zipWithIndex map { case (v,i) => (v, i+ ak.idMap.size + 1) })

    trans.exprs foreach println

    val cnf = trans.exprs flatMap { e => e.toCNF(idMap) }

    val sat = new SATBuilder(cnf, idMap.size,
                              (trans.genVars map idMap.apply).toSet)
    sat.isSatisfiable(assumps map {
      case Pos(id) => idMap(id)
      case Neg(id) => -idMap(id)
    })
  }

  @Test def simple {
    val k = """
      config A boolean {
        prompt "..." if []
      }
      """
    assert(isKConfigSAT(KConfigParser.parseKConfig(k).toAbstractKConfig))
  }

  @Test def twoPrompts {
    val k = """
      config A boolean {
        prompt "..." if []
        prompt "..." if []
      }
      """
    assert(isKConfigSAT(KConfigParser.parseKConfig(k).toAbstractKConfig))
  }

  /**
   * Both A and B are dead.
   */
  @Test def dead {
    val k = """
      config A boolean {
      }
      config B boolean {
        default [y] if [A]
      }
      """
    assert(isKConfigSAT(KConfigParser.parseKConfig(k).toAbstractKConfig))
    assert(!isKConfigSAT(KConfigParser.parseKConfig(k).toAbstractKConfig, "A"))
    assert(!isKConfigSAT(KConfigParser.parseKConfig(k).toAbstractKConfig, "B"))
  }

  @Test def equivalence {
    val k = """
      config A boolean {
        default [B] if []
      }
      config B boolean {
        default [y] if []
      }
      """
    assert(isKConfigSAT(KConfigParser.parseKConfig(k).toAbstractKConfig))
    assert(!isKConfigSAT(KConfigParser.parseKConfig(k).toAbstractKConfig, -"B"))
    assert(!isKConfigSAT(KConfigParser.parseKConfig(k).toAbstractKConfig, -"A"))
  }

  @Test def unsat {
    //TODO FIXME
  }

}