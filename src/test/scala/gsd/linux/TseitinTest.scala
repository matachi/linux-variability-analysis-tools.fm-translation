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

import org.junit.Test

class TseitinTest extends Tseitin {

  implicit def toBId(s: String) = BId(s)

  @Test def testNot {
    assert(transform(!"A") == List(BId("x1"), "x1" iff !"x2", "x2" iff "A"))
  }

  @Test def testAtom {
    assert(transform("A") == List(BId("x1"), "x1" iff "A"))
  }

}