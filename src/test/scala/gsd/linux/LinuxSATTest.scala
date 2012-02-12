package gsd.linux

import cnf.{DimacsReader, SATBuilder, CNFParser}
import org.junit.Test

class LinuxSATTest {

  def isDimacsSAT(file: String): Boolean = {
    println("Reading %s...".format(file))
    val (header, problem) =
      (DimacsReader.readHeaderFile(file), DimacsReader.readFile(file))

    val sat = new SATBuilder(problem.cnf, problem.numVars, header.generated)
    sat.isSatisfiable
  }

  // FIXME add some test cases

}

