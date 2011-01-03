package gsd.linux

import cnf.{DimacsReader, SATBuilder, CNFParser}
import org.junit.Test

/**
 * This class is excluded by default in the 'mvn test' target. See pom.xml.
 */
class LinuxSATTest {

  val formulasDir = "../formulas"

  def isDimacsSAT(file: String): Boolean = {
    println("Reading %s...".format(file))
    val (header, problem) =
      (DimacsReader.readHeaderFile(file), DimacsReader.readFile(file))

    val sat = new SATBuilder(problem.cnf, problem.numVars, header.generated)
    sat.isSatisfiable
  }

  @Test
  def v2_6_32_1var: Unit =
    assert(isDimacsSAT(formulasDir + "/2.6.32-1var.dimacs"))

  @Test
  def v2_6_32_2var: Unit =
    assert(isDimacsSAT(formulasDir + "/2.6.32-2var.dimacs"))

  @Test
  def v2_6_33_3_1var: Unit =
    assert(isDimacsSAT(formulasDir + "/2.6.33.3-1var.dimacs"))

  @Test
  def v2_6_33_3_2var: Unit =
    assert(isDimacsSAT(formulasDir + "/2.6.33.3-2var.dimacs"))
}