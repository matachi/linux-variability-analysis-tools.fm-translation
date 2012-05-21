/*
 * This file is part of the Linux Variability Modeling Tools (LVAT).
 *
 * Copyright (C) 2010 Steven She <shshe@gsd.uwaterloo.ca>
 *
 * LVAT is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * LVAT is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LVAT.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

package gsd.linux

object KConfigUtil {

  /**
   * Chunks defaults such that each list of defaults has the same consecutive
   * default values (ie. Default.iv)
   */
  def chunkDefaults(defs: List[Default]): List[List[Default]] = defs match {
    case Nil => Nil
    case Default(iv, _) :: _ =>
      val (same, rest) = defs partition { _.iv == iv }
      same :: chunkDefaults(rest)
  }
}
