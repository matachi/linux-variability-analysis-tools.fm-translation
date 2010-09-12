package gsd.linux

object KConfigUtil {

  /**
   * Chunks defaults such that each list of defaults has the same consecutive
   * default values (ie. Default.iv)
   *
   * TODO move into separate object
   */
  def chunkDefaults(defs: List[Default]): List[List[Default]] = defs match {
    case Nil => Nil
    case Default(iv, _) :: _ =>
      val (same, rest) = defs partition { _.iv == iv }
      same :: chunkDefaults(rest)
  }
}
