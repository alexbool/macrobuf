package me.alexbool.macrobuf.reflection

import com.google.protobuf.{WireFormat, CodedOutputStream}
import WireFormat._

trait FieldSerializer[T] {
  def serialize(number: Int, value: T, out: CodedOutputStream) {
    serializeTag(number, out)
    serializeValue(value, out)
  }
  def serializeTag(number: Int, out: CodedOutputStream)
  def serializeValue(value: T, out: CodedOutputStream)

  def size(number: Int, value: T): Int = tagSize(number) + valueSize(value)
  def tagSize(number: Int): Int = CodedOutputStream.computeTagSize(number)
  def valueSize(value: T): Int
}

object FieldSerializers {

  def intSerializer = new FieldSerializer[Int] {
    def serializeTag(number: Int, out: CodedOutputStream) {
      out.writeTag(number, WIRETYPE_VARINT)
    }

    def serializeValue(value: Int, out: CodedOutputStream) {
      out.writeInt32NoTag(value)
    }

    def valueSize(value: Int) = CodedOutputStream.computeInt32SizeNoTag(value)
  }

  def longSerializer = new FieldSerializer[Long] {
    def serializeTag(number: Int, out: CodedOutputStream) {
      out.writeTag(number, WIRETYPE_VARINT)
    }

    def serializeValue(value: Long, out: CodedOutputStream) {
      out.writeInt64NoTag(value)
    }

    def valueSize(value: Long) = CodedOutputStream.computeInt64SizeNoTag(value)
  }

  def shortSerializer = new FieldSerializer[Short] {
    def serializeTag(number: Int, out: CodedOutputStream) {
      out.writeTag(number, WIRETYPE_VARINT)
    }

    def serializeValue(value: Short, out: CodedOutputStream) {
      out.writeInt32NoTag(value)
    }

    def valueSize(value: Short) = CodedOutputStream.computeInt32SizeNoTag(value)
  }

  def booleanSerializer = new FieldSerializer[Boolean] {
    def serializeTag(number: Int, out: CodedOutputStream) {
      out.writeTag(number, WIRETYPE_VARINT)
    }

    def serializeValue(value: Boolean, out: CodedOutputStream) {
      out.writeBoolNoTag(value)
    }

    def valueSize(value: Boolean) = CodedOutputStream.computeBoolSizeNoTag(value)
  }

  def floatSerializer = new FieldSerializer[Float] {
    def serializeTag(number: Int, out: CodedOutputStream) {
      out.writeTag(number, WIRETYPE_FIXED32)
    }

    def serializeValue(value: Float, out: CodedOutputStream) {
      out.writeFloatNoTag(value)
    }

    def valueSize(value: Float) = CodedOutputStream.computeFloatSizeNoTag(value)
  }

  def doubleSerializer = new FieldSerializer[Double] {
    def serializeTag(number: Int, out: CodedOutputStream) {
      out.writeTag(number, WIRETYPE_FIXED64)
    }

    def serializeValue(value: Double, out: CodedOutputStream) {
      out.writeDoubleNoTag(value)
    }

    def valueSize(value: Double) = CodedOutputStream.computeDoubleSizeNoTag(value)
  }

  def stringSerializer = new FieldSerializer[String] {
    def serializeTag(number: Int, out: CodedOutputStream) {
      out.writeTag(number, WIRETYPE_LENGTH_DELIMITED)
    }

    def serializeValue(value: String, out: CodedOutputStream) {
      out.writeStringNoTag(value)
    }

    def valueSize(value: String) = CodedOutputStream.computeStringSizeNoTag(value)
  }

  def optional[T](underlying: FieldSerializer[T]) = new FieldSerializer[Option[T]] {
    override def serialize(number: Int, value: Option[T], out: CodedOutputStream) {
      value foreach { v =>
        underlying.serialize(number, v, out)
      }
    }

    def serializeTag(number: Int, out: CodedOutputStream) { ??? }
    def serializeValue(value: Option[T], out: CodedOutputStream) { ??? }

    override def size(number: Int, value: Option[T]) = value match {
      case Some(v) => underlying.size(number, v)
      case None    => 0
    }

    def valueSize(value: Option[T]) = ???
  }

  def repeated[T](underlying: FieldSerializer[T]) = new FieldSerializer[Seq[T]] {
    override def serialize(number: Int, value: Seq[T], out: CodedOutputStream) {
      value foreach { v =>
        underlying.serialize(number, v, out)
      }
    }

    def serializeTag(number: Int, out: CodedOutputStream) { ??? }
    def serializeValue(value: Seq[T], out: CodedOutputStream) { ??? }

    override def size(number: Int, value: Seq[T]) = value.map(underlying.size(number, _)).sum
    def valueSize(value: Seq[T]) = ???
  }

  def packedRepeated[T](underlying: FieldSerializer[T]) = new FieldSerializer[Seq[T]] {
    def serializeTag(number: Int, out: CodedOutputStream) {
      out.writeTag(number, WIRETYPE_LENGTH_DELIMITED)
    }

    def serializeValue(value: Seq[T], out: CodedOutputStream) {
      out.writeRawVarint32(valueSize(value))
      value foreach { v =>
        underlying.serializeValue(v, out)
      }
    }

    override def tagSize(number: Int) = underlying.tagSize(number)
    def valueSize(value: Seq[T]) = value.map(underlying.valueSize).sum
  }
}

trait MessageFieldSerializer extends FieldSerializer[Any] {
  /**
   * Actually serialize message
   */
  def serialize(value: Any, out: CodedOutputStream)

  def serializeTag(number: Int, out: CodedOutputStream) {
    out.writeTag(number, WIRETYPE_LENGTH_DELIMITED)
  }

  def serializeValue(value: Any, out: CodedOutputStream) {
    out.writeRawVarint32(valueSize(value))
    serialize(value, out)
  }
}
