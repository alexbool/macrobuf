package me.alexbool.macrobuf

import org.scalatest.WordSpec
import org.scalatest.Matchers
import Messages._

trait ParserSpec extends WordSpec with Matchers {

  def name: String
  def parserForMessage1: Parser[Message1]
  def parserForMessage2: Parser[Message2]
  def parserForMessage3: Parser[Message3]
  def parserForMessage4: Parser[Message4]
  def parserForMessage5: Parser[Message5]
  def parserForMessage6: Parser[Message6]
  def parserForMessage7: Parser[Message7]
  def parserForMessage8: Parser[Message8]
  def parserForMessage9: Parser[Message9]

  name should {
    "parse flat messages" in {
      val parser = parserForMessage1
      // https://developers.google.com/protocol-buffers/docs/encoding#simple
      parser.parse(Array(0x08, 0x96, 0x01).map(_.toByte)) should equal (Message1(150))
      parser.parse(Array(0x08, 0x00).map(_.toByte)) should equal (Message1(0))
    }
    "parse messages with strings" in {
      val parser = parserForMessage2
      // https://developers.google.com/protocol-buffers/docs/encoding#types
      parser.parse(Array(0x0a, 0x07, 0x74, 0x65, 0x73, 0x74, 0x69, 0x6e, 0x67).map(_.toByte)) should equal (Message2("testing"))
    }
    "parse messages with optional fields" in {
      val parser = parserForMessage3
      // https://developers.google.com/protocol-buffers/docs/encoding#optional
      parser.parse(Array(0x08, 0x96, 0x01).map(_.toByte)) should equal (Message3(Some(150)))
    }
    "parse messages with repeated fields" in {
      val parser = parserForMessage4
      // https://developers.google.com/protocol-buffers/docs/encoding#optional
      parser.parse(Array(0x08, 0x96, 0x01, 0x08, 0x00).map(_.toByte)) should equal (Message4(Seq(150, 0)))
    }
    "parse embedded messages" in {
      val parser = parserForMessage5
      parser.parse(Array(0x0a, 0x03, 0x08, 0x96, 0x01).map(_.toByte)) should equal (Message5(Message1(150)))
    }
    "parse repeated embedded messages" in {
      val parser = parserForMessage6
      parser.parse(Array(0x0a, 0x03, 0x08, 0x96, 0x01).map(_.toByte)) should equal (Message6(Seq(Message1(150))))
    }
    "parse optional embedded messages" in {
      val parser = parserForMessage7
      parser.parse(Array(0x0a, 0x03, 0x08, 0x96, 0x01).map(_.toByte)) should equal (Message7(Some(Message1(150))))
      parser.parse(Array[Byte]()) should equal (Message7(None))
    }
    "parse messages with several fields" in {
      val parser = parserForMessage8
      parser.parse(Array(0x08, 0x96, 0x01, 0x12, 0x07, 0x74, 0x65, 0x73, 0x74, 0x69, 0x6e, 0x67).map(_.toByte)) should equal (Message8(150, "testing"))
    }
    "parse messages with packed repeated fields" in {
      val parser = parserForMessage9
      parser.parse(Array(0x08, 0x96, 0x01, 0x12, 0x03, 0x01, 0x02, 0x03).map(_.toByte)) should equal (Message9(150, Seq(1, 2, 3)))
    }
    "parse lists of messages using delimeted format" in {
      val parser = parserForMessage1
      parser.parseDelimited(Array(0x03, 0x08, 0x96, 0x01, 0x02, 0x08, 0x00).map(_.toByte)) should equal (Seq(Message1(150), Message1(0)))
    }
  }
}
