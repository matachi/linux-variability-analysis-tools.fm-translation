package gsd.linux.tools

import org.clapper.argot.{ArgotConversionException, CommandLineArgument}

trait ArgotUtil {

  import Projects._
  
  implicit def convertProject(s: String,
                              opt: CommandLineArgument[FileBasedProject]): FileBasedProject =
    if (projects.keySet.contains(s)) projects(s)
    else throw new ArgotConversionException("%s is not a supported project".format(s))


}