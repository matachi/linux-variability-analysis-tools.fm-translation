package gsd.linux

import org.junit.Test

class ParserTest {

  @Test def testParser {
    KConfigParser.parseKConfigFile("../extracts/2.6.28.6.exconfig")
  }

}