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

  @Test def deadFeature {
    val k = """
    config A boolean
    """
    val bk = b.mkBDD(t(k))
    assert(bk === rootVar.andWith("A".not))
    bk.free
  }

  //TODO BDD Domain

}