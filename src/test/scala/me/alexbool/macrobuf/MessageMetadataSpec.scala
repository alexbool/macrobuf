package me.alexbool.macrobuf

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers
import MessageMetadata.runtime._

class MessageMetadataSpec extends WordSpec with MustMatchers {

  case class Message1(number: Int)
  case class Message2(numbers: Iterable[Int])
  case class Message3(name: String, msg: Option[Message2])
  case class Message4(name: Option[String], msgs: Iterable[Message1])
  case class Message5(msg: Message1)

  "RootMessage factory" must {
    "construct tree for plain types" in {
      val rm = MessageMetadata.runtime[Message1]
      rm.messageName must be ("Message1")
      rm.fields must have size (1)
      rm.fields.head.number must be (1)
      rm.fields.head.fieldName must be ("number")
      rm.fields.head.isInstanceOf[Primitive] must be (true)
      rm.fields.head.asInstanceOf[Primitive].optional must be (false)
    }
    "construct tree for types with repeated fields" in {
      val rm = MessageMetadata.runtime[Message2]
      rm.messageName must be ("Message2")
      rm.fields must have size (1)
      rm.fields.head.number must be (1)
      rm.fields.head.isInstanceOf[RepeatedPrimitive] must be (true)
    }
    "construct tree for types with optional fields" in {
      val rm = MessageMetadata.runtime[Message3]
      rm.messageName must be ("Message3")
      rm.fields must have size (2)
      rm.fields.head.isInstanceOf[Primitive] must be (true)
      rm.fields.head.asInstanceOf[Primitive].optional must be (false)
      rm.fields(1).isInstanceOf[EmbeddedMessage] must be (true)
      rm.fields(1).number must be (2)
      rm.fields(1).asInstanceOf[EmbeddedMessage].optional must be (true)
      rm.fields(1).asInstanceOf[EmbeddedMessage].messageName must be ("Message2")
    }
    "construct tree for types with repeated message fields" in {
      val rm = MessageMetadata.runtime[Message4]
      rm.messageName must be ("Message4")
      rm.fields must have size (2)
      rm.fields.head.number must be (1)
      rm.fields.head.fieldName must be ("name")
      rm.fields.head.isInstanceOf[Primitive] must be (true)
      rm.fields.head.asInstanceOf[Primitive].optional must be (true)
      rm.fields(1).isInstanceOf[RepeatedMessage] must be (true)
      rm.fields(1).number must be (2)
      rm.fields(1).asInstanceOf[RepeatedMessage].messageName must be ("Message1")
    }
    "construct tree for types with required embedded fields" in {
      val rm = MessageMetadata.runtime[Message5]
      rm.fields.head.isInstanceOf[EmbeddedMessage] must be (true)
      rm.fields.head.asInstanceOf[EmbeddedMessage].optional must be (false)
    }
  }
}
