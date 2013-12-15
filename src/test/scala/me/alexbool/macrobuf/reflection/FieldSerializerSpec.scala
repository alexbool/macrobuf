package me.alexbool.macrobuf.reflection

import scala.reflect.runtime.universe._
import org.scalatest.{WordSpec, Matchers}
import com.google.protobuf.CodedOutputStream
import java.io.ByteArrayOutputStream
import me.alexbool.macrobuf.MessageMetadata
import me.alexbool.macrobuf.Messages.Message1

class FieldSerializerSpec extends WordSpec with Matchers {
  val fieldNumber = 1

  "Field serializers" should {
    "work with embedded message fields" in {
      val serializer = new ReflectionMessageSerializer(MessageMetadata.runtime(typeOf[Message1]))
      test(serializer, Message1(0),   Array(0x0a, 0x02, 0x08, 0x00))
      test(serializer, Message1(150), Array(0x0a, 0x03, 0x08, 0x96, 0x01))
    }
  }

  private def test[T](serializer: FieldSerializer[T], obj: T, expectedResult: Array[Int]) {
    val bytes = new ByteArrayOutputStream()
    val out = CodedOutputStream.newInstance(bytes)
    serializer.serialize(fieldNumber, obj, out)
    out.flush()
    bytes.toByteArray should be (expectedResult.map(_.toByte))
    serializer.size(fieldNumber, obj) should be (bytes.toByteArray.length)
  }
}
