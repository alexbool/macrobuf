package me.alexbool.macrobuf.reflection

import org.scalatest.matchers.{MatchResult, Matcher}
import java.io.ByteArrayOutputStream
import com.google.protobuf.CodedOutputStream

trait FieldSerializerMatchers {

  val fieldNumber = 1

  class SerializeToMatcher[T](serializer: FieldSerializer[T], expectedResult: Array[Int]) extends Matcher[T] {
    def apply(left: T) = {
      val bytes = new ByteArrayOutputStream()
      val out = CodedOutputStream.newInstance(bytes)
      serializer.serialize(fieldNumber, left, out)
      out.flush()
      val expectedBytes = expectedResult.map(_.toByte)
      val resultIsExpected = bytes.toByteArray.deep == expectedBytes.deep
      val size = serializer.size(fieldNumber, left)
      val sizeIsExpected = size == bytes.toByteArray.length
      if (!resultIsExpected) {
        MatchResult(
          matches = false,
          rawFailureMessage = s"Field $left was serialized to ${bytes.toByteArray.deep}, but expected ${expectedBytes.deep}",
          rawNegatedFailureMessage = s"Field $left was serialized to ${bytes.toByteArray.deep}"
        )
      } else {
        MatchResult(
          matches = sizeIsExpected,
          rawFailureMessage = s"Field $left has size $size, but expected ${bytes.toByteArray.length}",
          rawNegatedFailureMessage = s"Field $left has size $size"
        )
      }
    }
  }

  def serializeTo[T](expectedResult: Array[Int])(implicit serializer: FieldSerializer[T]) =
    new SerializeToMatcher[T](serializer, expectedResult)
}
