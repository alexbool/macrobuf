package me.alexbool.macrobuf

import org.scalatest.Matchers
import org.scalatest.matchers.Matcher
import org.scalautils.Equality

trait ParserMatchers {

  import Matchers._

  def parseTo[T](expectedMessage: T)(implicit parser: Parser[T]): Matcher[Array[Int]] =
    equal(expectedMessage).matcher(Equality.default[T]) compose { (data: Array[Int]) =>
      parser.parse(data.map(_.toByte))
    }

  def parseDelimitedTo[T](expectedMessages: Seq[T])(implicit parser: Parser[T]): Matcher[Array[Int]] =
    equal(expectedMessages).matcher(Equality.default[Seq[T]]) compose { (data: Array[Int]) =>
      parser.parseDelimited(data.map(_.toByte))
    }
}
