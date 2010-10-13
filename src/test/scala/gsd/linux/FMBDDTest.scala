package gsd.linux

import org.junit.{Test, Before}

class FMBDDTest {

  val b = new BDDBuilder(
    Map() ++ (('a' to 'z').toList.zipWithIndex
                  .map { case (k,v) => ("" + k, v + 1) })) with FMBDDBuilder

  def t(s: String): FM[B2Expr] = {
    val ck = KConfigParser.parseKConfig(s)
    val parents = Hierarchy.mkParentMap(ck)
    BFMTranslation.mkFeatureModel(parents, ck)
  }

  @Test def simple {
    val k = """
    config A boolean
    """
    println(b.mkBDD(t(k)))
    assert(true)
  }

}