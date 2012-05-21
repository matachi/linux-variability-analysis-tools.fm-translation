package gsd.linux

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import stats.ConstraintStatistics

class ConstraintTest extends AssertionsForJUnit {

  import ConstraintStatistics._

  @Test
  def testSubsumption {
    assert(isSubsumedBy(Id("A"), Id("A")))
    assert(!isSubsumedBy(Id("A"), Id("A") && Id("B")))
    assert(isSubsumedBy(Id("A") && Id("B"), Id("A")))
    assert(!isSubsumedBy(Id("A"), Id("A") && Id("B") && Id("C")))
    assert(isSubsumedBy(Id("A") && Id("B") && Id("C"), Id("A")))
    assert(isSubsumedBy(Id("A"), Id("A") || Id("B")))
  }

}