package me.alexbool.macrobuf

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec
import scala.reflect.runtime.universe._

trait ProtobufSerializerSpec extends WordSpec with MustMatchers {

  def createSerializer[T: TypeTag]: Serializer[T]
  def createListSerializer[T: TypeTag]: Serializer[Iterable[T]]

  "Protobuf serializer" must {
    "serialize flat messages" in {
      val serializer = createSerializer[Message1]
      // https://developers.google.com/protocol-buffers/docs/encoding#simple
      serializer.serialize(Message1(150)) must equal (Array(0x08, 0x96, 0x01).map(_.toByte))
      serializer.serialize(Message1(0)) must equal (Array(0x08, 0x00).map(_.toByte))
    }
    "serialize messages with strings" in {
      val serializer = createSerializer[Message2]
      // https://developers.google.com/protocol-buffers/docs/encoding#types
      serializer.serialize(Message2("testing")) must equal (Array(0x0a, 0x07, 0x74, 0x65, 0x73, 0x74, 0x69, 0x6e, 0x67).map(_.toByte))
    }
    "serialize messages with optional fields" in {
      val serializer = createSerializer[Message3]
      // https://developers.google.com/protocol-buffers/docs/encoding#optional
      serializer.serialize(Message3(Some(150))) must equal (Array(0x08, 0x96, 0x01).map(_.toByte))
    }
    "serialize messages with repeated fields" in {
      val serializer = createSerializer[Message4]
      // https://developers.google.com/protocol-buffers/docs/encoding#optional
      serializer.serialize(Message4(Seq(150, 0))) must equal (Array(0x08, 0x96, 0x01, 0x08, 0x00).map(_.toByte))
    }
    "serialize embedded messages" in {
      val serializer = createSerializer[Message5]
      serializer.serialize(Message5(Message1(150))) must equal (Array(0x0a, 0x03, 0x08, 0x96, 0x01).map(_.toByte))
    }
    "serialize repeated embedded messages" in {
      val serializer = createSerializer[Message6]
      serializer.serialize(Message6(Seq(Message1(150)))) must equal (Array(0x0a, 0x03, 0x08, 0x96, 0x01).map(_.toByte))
    }
    "serialize optional embedded messages" in {
      val serializer = createSerializer[Message7]
      serializer.serialize(Message7(Some(Message1(150)))) must equal (Array(0x0a, 0x03, 0x08, 0x96, 0x01).map(_.toByte))
      serializer.serialize(Message7(None)) must equal (Array[Byte]())
    }
    "serialize lists of messages using delimeted format" in {
      val serializer = createListSerializer[Message1]
      serializer.serialize(Seq(Message1(150), Message1(0))) must equal (Array(0x03, 0x08, 0x96, 0x01, 0x02, 0x08, 0x00).map(_.toByte))
    }
  }
}
