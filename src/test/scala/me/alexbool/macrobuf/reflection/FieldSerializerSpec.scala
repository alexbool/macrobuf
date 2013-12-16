package me.alexbool.macrobuf.reflection

import scala.reflect.runtime.universe._
import org.scalatest.{WordSpec, Matchers}
import com.google.protobuf.CodedOutputStream
import java.io.ByteArrayOutputStream
import me.alexbool.macrobuf.MessageMetadata
import me.alexbool.macrobuf.Messages.Message1

class FieldSerializerSpec extends WordSpec with Matchers with FieldSerializerMatchers {

  "Field serializers" should {
    "work with embedded message fields" in {
      implicit val serializer = new ReflectionMessageSerializer(MessageMetadata.runtime(typeOf[Message1]))
      Message1(0)   should serializeTo (Array(0x0a, 0x02, 0x08, 0x00))
      Message1(150) should serializeTo (Array(0x0a, 0x03, 0x08, 0x96, 0x01))
    }
  }
}
