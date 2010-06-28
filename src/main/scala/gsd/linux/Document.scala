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

  implicit def string(s: String) = StringText(s)
  implicit def concat(lst: Iterable[Text]): Text = iterToText(lst)(_ +: _)
  
  def iterToText(lst: Iterable[Text])(op: (Text, Text) => Text): Text =
    ((Empty: Text) /: lst)(op)

  type State = (Int, Text)

  abstract class Text {

    def ::(head: Text): Text = ConsText(head, SpaceText(this))
    def +:(head: Text): Text = ConsText(head, this)
    def :/:(head: Text) = head :: NewLine +: this

    def format(w: PrintStream) = {

      def spaces(level: Int): String =
        ((0 until level*2) map Function.const(' ')).mkString

      def fmt(level: Int, state: List[Text]): Unit = state match {
        case Nil =>
          //Do nothing
        case NewLine::Nil =>
          //Do nothing
        case NewLine::tail =>
          w.println
          w.print(spaces(level))
          fmt(level, tail)
        case ConsText(t1, t2)::tail =>
          fmt(level, t1 :: t2 :: tail)
        case BlockText(begin,end,Empty)::tail =>
          w.print(begin + " " + end)
          fmt(level, tail)
        case BlockText(begin,end,t)::tail =>
          w.print(begin)
          fmt(level+1, NewLine :: t :: Nil)
          fmt(level, NewLine :: string(end) :: tail)
        case SpaceText(t)::tail =>
          w.print(" ")
          fmt(level, t :: tail)
        case StringText(s)::tail =>
          w.print(s)
          fmt(level, tail)
        case Empty::tail =>
          fmt(level, tail)

      }

      fmt(0, this :: Nil)
    }

  }

  case class BlockText(beginSep: String, endSep: String, t: Text) extends Text
  case class StringText(s: String) extends Text
  case class SpaceText(t: Text) extends Text
  case class ConsText(head: Text, tail: Text) extends Text
  case object NewLine extends Text
  case object Empty extends Text { //not really necessary
    override def ::(head: Text) = head
    override def :/:(head: Text) = head :: NewLine
  }

}

trait FMDocument {

  import Document._

  //TODO groups
  def toText(fm: FeatureModel): Text =
    BlockText("", "", fm.features map toText)

  def toText(f: Feature): Text = f match {
    case OptFeature(name,t,ctcs,cs) =>
      name :: "?" :: ":" :: toText(t) :: BlockText("{", "}", cs map toText) :/:
        BlockText("[", "]", iterToText(ctcs map toText)(_ :/: _)) :: NewLine
    case MandFeature(name,t,ctcs,cs) =>
      name :: ":" :: toText(t) :: BlockText("{", "}", cs map toText) :: NewLine
  }

  def toText(t: FeatType): Text = t match {
    case BoolFeat => "boolean"
    case TriFeat => "tristate"
    case StringFeat => "string"
    case IntFeat => "int"
  }

  def toText(e: TExpr): Text = e match {
    case TYes => "Y"
    case TMod => "M"
    case TNo => "N"
    case TAnd(x,y) => "(" +: toText(x) +: ")" :: "&" :: "(" +: toText(y) +: ")"
    case TOr(x,y) => "(" +: toText(x) +: ")" :: "|" :: "(" +: toText(y) +: ")"
    case TEq(x,y) => "(" +: toText(x) +: ")" :: "=" :: "(" +: toText(y) +: ")"
    case TGte(x,y) => "(" +: toText(x) +: ")" :: ">=" :: "(" +: toText(y) +: ")"
    case TLte(x,y) => "(" +: toText(x) +: ")" :: "<=" :: "(" +: toText(y) +: ")"

    case TNot(x) => "!" +: toText(x)
    case TBool(x) => "Bool(" +: toText(x) +: string(")")
    case TId(x) => string(x)

    case _ => StringText(e.toString)
  }

}

