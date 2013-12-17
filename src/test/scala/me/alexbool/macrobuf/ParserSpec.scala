package me.alexbool.macrobuf

import org.scalatest.WordSpec
import org.scalatest.Matchers
import Messages._

trait ParserSpec extends WordSpec with Matchers with ParserMatchers {

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
      implicit val parser = parserForMessage1
      // https://developers.google.com/protocol-buffers/docs/encoding#simple
      Array(0x08, 0x96, 0x01) should parseTo (Message1(150))
      Array(0x08, 0x00)       should parseTo (Message1(0))
    }
    "parse messages with strings" in {
      implicit val parser = parserForMessage2
      // https://developers.google.com/protocol-buffers/docs/encoding#types
      Array(0x0a, 0x07, 0x74, 0x65, 0x73, 0x74, 0x69, 0x6e, 0x67) should parseTo (Message2("testing"))
    }
    "parse messages with optional fields" in {
      implicit val parser = parserForMessage3
      // https://developers.google.com/protocol-buffers/docs/encoding#optional
      Array(0x08, 0x96, 0x01) should parseTo (Message3(Some(150)))
    }
    "parse messages with repeated fields" in {
      implicit val parser = parserForMessage4
      // https://developers.google.com/protocol-buffers/docs/encoding#optional
      Array(0x08, 0x96, 0x01, 0x08, 0x00) should parseTo (Message4(Seq(150, 0)))
    }
    "parse embedded messages" in {
      implicit val parser = parserForMessage5
      Array(0x0a, 0x03, 0x08, 0x96, 0x01) should parseTo (Message5(Message1(150)))
    }
    "parse repeated embedded messages" in {
      implicit val parser = parserForMessage6
      Array(0x0a, 0x03, 0x08, 0x96, 0x01, 0x0a, 0x02, 0x08, 0x00) should parseTo (
        Message6(Seq(Message1(150), Message1(0)))
      )
    }
    "parse optional embedded messages" in {
      implicit val parser = parserForMessage7
      Array(0x0a, 0x03, 0x08, 0x96, 0x01) should parseTo (Message7(Some(Message1(150))))
      Array[Int]()                        should parseTo (Message7(None))
    }
    "parse messages with several fields" in {
      implicit val parser = parserForMessage8
      Array(0x08, 0x96, 0x01, 0x12, 0x07, 0x74, 0x65, 0x73, 0x74, 0x69, 0x6e, 0x67) should parseTo (
        Message8(150, "testing")
      )
    }
    "parse messages with packed repeated fields" in {
      implicit val parser = parserForMessage9
      Array(0x08, 0x96, 0x01, 0x12, 0x03, 0x01, 0x02, 0x03) should parseTo (Message9(150, Seq(1, 2, 3)))
    }
    "parse lists of messages using delimeted format" in {
      implicit val parser = parserForMessage1
      Array(0x03, 0x08, 0x96, 0x01, 0x02, 0x08, 0x00) should parseDelimitedTo (Seq(Message1(150), Message1(0)))
    }
  }
}
