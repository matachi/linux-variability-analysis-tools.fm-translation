package gsd.linux

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

class TExprTest extends AssertionsForJUnit {

  @Test def eq {
    expect(BTrue)((TYes beq TYes).simplify)
    expect(BTrue)((TMod beq TMod).simplify)
    expect(BTrue)((TNo beq TNo).simplify)
    expect(BFalse)((TYes beq TMod).simplify)
    expect(BFalse)((TMod beq TYes).simplify)
    expect(BFalse)((TNo beq TMod).simplify)
    expect(BFalse)((TNo beq TYes).simplify)
  }

  @Test def lte {
    expect(BTrue)((TNo <= TNo).simplify)
    expect(BFalse)((TMod <= TNo).simplify)
    expect(BFalse)((TYes <= TNo).simplify)

    expect(BTrue)((TNo <= TMod).simplify)
    expect(BTrue)((TMod <= TMod).simplify)
    expect(BFalse)((TYes <= TMod).simplify)

    expect(BTrue)((TNo <= TYes).simplify)
    expect(BTrue)((TMod <= TYes).simplify)
    expect(BTrue)((TYes <= TYes).simplify)
  }

  @Test def gt {
    expect(BFalse)((TNo > TNo).simplify)
    expect(BTrue)((TMod > TNo).simplify)
    expect(BTrue)((TYes > TNo).simplify)

    expect(BFalse)((TNo > TMod).simplify)
    expect(BFalse)((TMod > TMod).simplify)
    expect(BTrue)((TYes > TMod).simplify)

    expect(BFalse)((TNo > TYes).simplify)
    expect(BFalse)((TMod > TYes).simplify)
    expect(BFalse)((TYes > TYes).simplify)
  }

}