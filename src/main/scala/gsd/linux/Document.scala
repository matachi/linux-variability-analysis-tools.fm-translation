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

import java.io.PrintStream

/**
 * A simplified version of scala.text.Document for fun.
 */
object Document {

  implicit def string(s: String): Text = StringT(s)
  implicit def concat(lst: Iterable[Text]): Text = iterToText(lst)(_ +: _)
  
  def iterToText(lst: Iterable[Text])(op: (Text, Text) => Text): Text =
    ((Empty: Text) /: lst)(op)

  type State = (Int, Text)

  abstract class Text {

    def ::(head: Text): Text = ConsT(head, SpaceT(this))
    def +:(head: Text): Text = ConsT(head, this)
    def :/:(head: Text) = if (head == Empty) this
                          else head :: NL +: this

    def print() = format(System.out)

    def format(w: PrintStream) = {

      def spaces(level: Int): String =
        ((0 until level*2) map Function.const(' ')).mkString

      def fmt(level: Int, state: List[Text]): Unit = state match {
        case Nil =>
          //Do nothing
        case NL::Nil =>
          //Do nothing
        case NL::tail =>
          w.println
          w.print(spaces(level))
          fmt(level, tail)
        case ConsT(t1, t2)::tail =>
          fmt(level, t1 :: t2 :: tail)
        case Block(begin,end,Empty)::tail =>
          w.print(begin + " " + end)
          fmt(level, tail)
        case Block(begin,end,t)::tail =>
          if (begin != "") w.print(begin)
          val endb = if (end != "") string(end) else Empty
          fmt(level+1, NL :: t :: Nil)
          fmt(level, NL :: endb :: tail)
        case SpaceT(t)::tail =>
          w.print(" ")
          fmt(level, t :: tail)
        case StringT(s)::tail =>
          w.print(s)
          fmt(level, tail)
        case Empty::tail =>
          fmt(level, tail)

      }

      fmt(0, this :: Nil)
      w.println
    }

  }

  case class Block(beginSep: String, endSep: String, t: Text) extends Text
  case class StringT(s: String) extends Text
  case class SpaceT(t: Text) extends Text
  case class ConsT(head: Text, tail: Text) extends Text
  case object NL extends Text
  case object Empty extends Text
}


