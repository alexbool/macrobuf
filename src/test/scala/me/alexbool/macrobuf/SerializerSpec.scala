package me.alexbool.macrobuf

import scala.reflect.runtime.universe._
import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec
import Messages._

trait SerializerSpec extends WordSpec with MustMatchers {

  def name: String
  def serializerForMessage1: Serializer[Message1]
  def serializerForMessage2: Serializer[Message2]
  def serializerForMessage3: Serializer[Message3]
  def serializerForMessage4: Serializer[Message4]
  def serializerForMessage5: Serializer[Message5]
  def serializerForMessage6: Serializer[Message6]
  def serializerForMessage7: Serializer[Message7]
  def serializerForMessage8: Serializer[Message8]
  def listSerializerForMessage1: Serializer[Iterable[Message1]]

  name must {
    "serialize flat messages" in {
      val serializer = serializerForMessage1
      // https://developers.google.com/protocol-buffers/docs/encoding#simple
      serializer.serialize(Message1(150)) must equal (Array(0x08, 0x96, 0x01).map(_.toByte))
      serializer.serialize(Message1(0)) must equal (Array(0x08, 0x00).map(_.toByte))
    }
    "serialize messages with strings" in {
      val serializer = serializerForMessage2
      // https://developers.google.com/protocol-buffers/docs/encoding#types
      serializer.serialize(Message2("testing")) must equal (Array(0x0a, 0x07, 0x74, 0x65, 0x73, 0x74, 0x69, 0x6e, 0x67).map(_.toByte))
    }
    "serialize messages with optional fields" in {
      val serializer = serializerForMessage3
      // https://developers.google.com/protocol-buffers/docs/encoding#optional
      serializer.serialize(Message3(Some(150))) must equal (Array(0x08, 0x96, 0x01).map(_.toByte))
    }
    "serialize messages with repeated fields" in {
      val serializer = serializerForMessage4
      // https://developers.google.com/protocol-buffers/docs/encoding#optional
      serializer.serialize(Message4(Seq(150, 0))) must equal (Array(0x08, 0x96, 0x01, 0x08, 0x00).map(_.toByte))
    }
    "serialize embedded messages" in {
      val serializer = serializerForMessage5
      serializer.serialize(Message5(Message1(150))) must equal (Array(0x0a, 0x03, 0x08, 0x96, 0x01).map(_.toByte))
    }
    "serialize repeated embedded messages" in {
      val serializer = serializerForMessage6
      serializer.serialize(Message6(Seq(Message1(150)))) must equal (Array(0x0a, 0x03, 0x08, 0x96, 0x01).map(_.toByte))
    }
    "serialize optional embedded messages" in {
      val serializer = serializerForMessage7
      serializer.serialize(Message7(Some(Message1(150)))) must equal (Array(0x0a, 0x03, 0x08, 0x96, 0x01).map(_.toByte))
      serializer.serialize(Message7(None)) must equal (Array[Byte]())
    }
    "serialize messages with several fields" in {
      val serializer = serializerForMessage8
      serializer.serialize(Message8(150, "testing")) must equal (Array(0x08, 0x96, 0x01, 0x12, 0x07, 0x74, 0x65, 0x73, 0x74, 0x69, 0x6e, 0x67).map(_.toByte))
    }
    "serialize lists of messages using delimeted format" in {
      val serializer = listSerializerForMessage1
      serializer.serialize(Seq(Message1(150), Message1(0))) must equal (Array(0x03, 0x08, 0x96, 0x01, 0x02, 0x08, 0x00).map(_.toByte))
    }
  }
}
