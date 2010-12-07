package gsd.linux.tools

import org.clapper.argot.{ArgotParser, ArgotConversionException, CommandLineArgument}

trait ArgotUtil {

  import Projects._
  
  implicit def convertProject(s: String,
                              opt: CommandLineArgument[FileBasedProject]): FileBasedProject =
    if (projects.keySet.contains(s)) projects(s)
    else throw new ArgotConversionException("%s is not a supported project".format(s))

  val name: String
  val preUsage: Option[String] = None

  lazy val parser = new ArgotParser(name, preUsage = preUsage)

  lazy val pOpt = parser.option[FileBasedProject](List("p", "project"), "pro",
    "supported projects: %s".format(projects.keySet.mkString(",")))

}


