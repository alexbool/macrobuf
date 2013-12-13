package me.alexbool.macrobuf

import org.scalatest.Matchers
import org.scalatest.WordSpec
import Messages._

trait SerializerSpec extends WordSpec with Matchers {

  def name: String
  def serializerForMessage1: Serializer[Message1]
  def serializerForMessage2: Serializer[Message2]
  def serializerForMessage3: Serializer[Message3]
  def serializerForMessage4: Serializer[Message4]
  def serializerForMessage5: Serializer[Message5]
  def serializerForMessage6: Serializer[Message6]
  def serializerForMessage7: Serializer[Message7]
  def serializerForMessage8: Serializer[Message8]
  def serializerForMessage9: Serializer[Message9]

  name should {
    "serialize flat messages" in {
      val serializer = serializerForMessage1
      // https://developers.google.com/protocol-buffers/docs/encoding#simple
      testSerialize(serializer, Message1(150), Array(0x08, 0x96, 0x01))
      testSerialize(serializer, Message1(0), Array(0x08, 0x00))
    }
    "serialize messages with strings" in {
      val serializer = serializerForMessage2
      // https://developers.google.com/protocol-buffers/docs/encoding#types
      testSerialize(serializer, Message2("testing"), Array(0x0a, 0x07, 0x74, 0x65, 0x73, 0x74, 0x69, 0x6e, 0x67))
    }
    "serialize messages with optional fields" in {
      val serializer = serializerForMessage3
      // https://developers.google.com/protocol-buffers/docs/encoding#optional
      testSerialize(serializer, Message3(Some(150)), Array(0x08, 0x96, 0x01))
    }
    "serialize messages with repeated fields" in {
      val serializer = serializerForMessage4
      // https://developers.google.com/protocol-buffers/docs/encoding#optional
      testSerialize(serializer, Message4(Seq(150, 0)), Array(0x08, 0x96, 0x01, 0x08, 0x00))
    }
    "serialize embedded messages" in {
      val serializer = serializerForMessage5
      testSerialize(serializer, Message5(Message1(150)), Array(0x0a, 0x03, 0x08, 0x96, 0x01))
    }
    "serialize repeated embedded messages" in {
      val serializer = serializerForMessage6
      testSerialize(
        serializer,
        Message6(Seq(Message1(150), Message1(0))),
        Array(0x0a, 0x03, 0x08, 0x96, 0x01, 0x0a, 0x02, 0x08, 0x00)
      )
    }
    "serialize optional embedded messages" in {
      val serializer = serializerForMessage7
      testSerialize(serializer, Message7(Some(Message1(150))), Array(0x0a, 0x03, 0x08, 0x96, 0x01))
      testSerialize(serializer, Message7(None),                Array())
    }
    "serialize messages with several fields" in {
      val serializer = serializerForMessage8
      testSerialize(
        serializer,
        Message8(150, "testing"),
        Array(0x08, 0x96, 0x01, 0x12, 0x07, 0x74, 0x65, 0x73, 0x74, 0x69, 0x6e, 0x67)
      )
    }
    "serialize messages with packed repeated fields" in {
      val serializer = serializerForMessage9
      testSerialize(
        serializer,
        Message9(150, Seq(1, 2, 3)), Array(0x08, 0x96, 0x01, 0x12, 0x03, 0x01, 0x02, 0x03)
      )
    }
    "serialize lists of messages using delimeted format" in {
      val serializer = serializerForMessage1
      testSerializeDelimited(
        serializer,
        Seq(Message1(150), Message1(0)),
        Array(0x03, 0x08, 0x96, 0x01, 0x02, 0x08, 0x00)
      )
    }
  }

  private def testSerialize[T](serializer: Serializer[T], message: T, expectedResult: Array[Int]) {
    serializer.serialize(message) should equal (expectedResult.map(_.toByte))
  }

  private def testSerializeDelimited[T](serializer: Serializer[T], messages: Seq[T], expectedResult: Array[Int]) {
    serializer.serializeDelimited(messages) should equal (expectedResult.map(_.toByte))
  }
}
