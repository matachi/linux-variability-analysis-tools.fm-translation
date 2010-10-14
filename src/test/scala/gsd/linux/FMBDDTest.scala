package gsd.linux

import org.junit.{Test, Before}
import org.scalatest.junit.AssertionsForJUnit
import net.sf.javabdd.BDD

class FMBDDTest extends AssertionsForJUnit {

  val rootVar = "Linux Kernel Configuration"

  val b = new BDDBuilder(
    Map() ++ ( (rootVar :: ('A' to 'Z').toList)
                  .zipWithIndex
                  .map { case (k,v) => (k.toString, v + 1) })) with FMBDDBuilder

  def t(s: String): FM[B2Expr] = {
    val ck = KConfigParser.parseKConfig(s)
    val parents = Hierarchy.mkParentMap(ck)
    BFMTranslation.mkFeatureModel(parents, ck)
  }

  implicit def toBDD(s: String): BDD = b ithVar s

  @Test def prompt {
    val k = """
    config A boolean {
      prompt "..." if []
    }
    """
    val bk = b.mkBDD(t(k))
    assert(bk === b.ithVar(rootVar))
    bk.free
  }

  @Test def dead {
    val k = """
    config A boolean
    """
    val bk = b.mkBDD(t(k))
    assert(bk === rootVar.andWith("A".not))
    bk.free
  }

  @Test def nesting {
    val k = """
    config A boolean {
      prompt "..." if []
      config B boolean {
        prompt "..." if []
      }
    }
    """
    val bk = b.mkBDD(t(k))
    assert(bk === rootVar.andWith("B" impWith "A"))
    bk.free
  }

  @Test def menu {
    val k = """
    config A boolean {
      prompt "..." if []
      menu "B" {
      }
    }
    """
    val bk = b.mkBDD(t(k))
    assert(bk === rootVar.andWith("B" biimpWith "A"))
    bk.free
  }

  @Test def derivedConfig {
    val k = """
    config A boolean {
      default [y] if [B]
    }
    config B boolean {
      prompt "..." if []
    }
    """
    val bk = b.mkBDD(t(k))
    assert(bk === rootVar.andWith("B" biimpWith "A"))
    bk.free
  }

  @Test def reverseDependency1 {
    val k = """
    config A boolean
    config B boolean {
      prompt "..." if []
      select A if []
    }
    """
    val bk = b.mkBDD(t(k))
    assert(bk === rootVar.andWith("B" biimpWith "A"))
    bk.free
  }

  @Test def reverseDependency2 {
    val k = """
    config A boolean
    config B boolean {
      prompt "..." if []
      select A if []
    }
    config C boolean {
      prompt "..." if []
      select A if []
    }
    """
    val bk = b.mkBDD(t(k))
    assert(bk === rootVar.andWith("A" biimpWith ("B" orWith "C")))
    bk.free
  }

  @Test def conditionalPrompt {
    val k = """
    config A boolean {
      prompt "..." if [B]
    }
    config B boolean {
      prompt "..." if []
    }
    """
    val bk = b.mkBDD(t(k))
    assert(bk === rootVar.andWith (
        ("A" andWith "B") orWith "A".not
      ))
    bk.free
  }

  /**
   * Default 'y' if B is equivalent to Default 'B'
   */
  @Test def multipleDefaults {
    val k = """
    config A boolean {
      default [y] if [B]
      default [y] if [C]
    }
    config B boolean {
      prompt "..." if []
    }
    config C boolean {
      prompt "..." if []
    }
    """
    val bk = b.mkBDD(t(k))
    assert(bk === rootVar.andWith (
      ("A" biimpWith ("B" orWith ("B".not andWith "C"))) // redundant but clearer
      ))
    bk.free
  }

  @Test def promptAndDefaults {
    val k = """
    config A boolean {
      prompt "..." if [B]
      default [y] if [C]
      default [y] if [D]
    }
    config B boolean {
      prompt "..." if []
    }
    config C boolean {
      prompt "..." if []
    }
    config D boolean {
      prompt "..." if []
    }
    """
    val bk = b.mkBDD(t(k))
    assert(bk === rootVar.andWith (
      "B" orWith ("A" biimpWith ("C" orWith ("C".not andWith "D"))) // redundant but clearer
      ))
    bk.free
  }

  //TODO Tristate tests

}