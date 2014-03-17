package me.alexbool.macrobuf

import org.scalatest.WordSpec
import org.scalatest.Matchers
import MessageMetadata.runtime._
import me.alexbool.macrobuf.annotation.packed

class MessageMetadataSpec extends WordSpec with Matchers {

  case class Message1(number: Int)
  case class Message2(numbers: Iterable[Int])
  case class Message3(name: String, msg: Option[Message2])
  case class Message4(name: Option[String], msgs: Iterable[Message1])
  case class Message5(msg: Message1)
  case class Message6(@packed numbers: Iterable[Int])
  case class Message7(@packed numbers: Iterable[String])

  "RootMessage factory" should {
    "construct tree for plain types" in {
      val rm = MessageMetadata.runtime[Message1]
      rm.messageName should be ("Message1")
      rm.fields should have size 1
      rm.fields.head.number should be (1)
      rm.fields.head.fieldName should be ("number")
      rm.fields.head.isInstanceOf[RequiredPrimitive] should be (true)
    }
    "construct tree for types with repeated fields" in {
      val rm = MessageMetadata.runtime[Message2]
      rm.messageName should be ("Message2")
      rm.fields should have size 1
      rm.fields.head.number should be (1)
      rm.fields.head.isInstanceOf[RepeatedPrimitive] should be (true)
    }
    "construct tree for types with optional fields" in {
      val rm = MessageMetadata.runtime[Message3]
      rm.messageName should be ("Message3")
      rm.fields should have size 2
      rm.fields.head.isInstanceOf[RequiredPrimitive] should be (true)
      rm.fields(1).isInstanceOf[OptionalMessage] should be (true)
      rm.fields(1).number should be (2)
      rm.fields(1).asInstanceOf[OptionalMessage].messageName should be ("Message2")
    }
    "construct tree for types with repeated message fields" in {
      val rm = MessageMetadata.runtime[Message4]
      rm.messageName should be ("Message4")
      rm.fields should have size 2
      rm.fields.head.number should be (1)
      rm.fields.head.fieldName should be ("name")
      rm.fields.head.isInstanceOf[OptionalPrimitive] should be (true)
      rm.fields(1).isInstanceOf[RepeatedMessage] should be (true)
      rm.fields(1).number should be (2)
      rm.fields(1).asInstanceOf[RepeatedMessage].messageName should be ("Message1")
    }
    "construct tree for types with required embedded fields" in {
      val rm = MessageMetadata.runtime[Message5]
      rm.fields.head.isInstanceOf[EmbeddedMessage] should be (true)
    }
    "handle packed repeated fields" in {
      val rm = MessageMetadata.runtime[Message6]
      rm.fields.head.isInstanceOf[RepeatedPrimitive] should be (true)
      rm.fields.head.asInstanceOf[RepeatedPrimitive].packed should be (true)
      an [IllegalArgumentException] should be thrownBy { MessageMetadata.runtime[Message7] }
    }
  }
}
