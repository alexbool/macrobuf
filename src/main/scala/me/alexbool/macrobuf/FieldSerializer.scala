package me.alexbool.macrobuf

import com.google.protobuf.{WireFormat, CodedOutputStream}

trait FieldSerializer[T] {
  def serialize(number: Int, value: T, out: CodedOutputStream)
  def size(number: Int, value: T): Int
}

object FieldSerializers {

  val IntSerializer = new FieldSerializer[Int] {
    def serialize(number: Int, value: Int, out: CodedOutputStream) {
      out.writeInt64(number, value)
    }
    def size(number: Int, value: Int) = CodedOutputStream.computeInt32Size(number, value)
  }

  val LongSerializer = new FieldSerializer[Long] {
    def serialize(number: Int, value: Long, out: CodedOutputStream) {
      out.writeInt64(number, value)
    }
    def size(number: Int, value: Long) = CodedOutputStream.computeInt64Size(number, value)
  }

  val ShortSerializer = new FieldSerializer[Short] {
    def serialize(number: Int, value: Short, out: CodedOutputStream) {
      out.writeInt32(number, value)
    }
    def size(number: Int, value: Short) = CodedOutputStream.computeInt32Size(number, value)
  }

  val BooleanSerializer = new FieldSerializer[Boolean] {
    def serialize(number: Int, value: Boolean, out: CodedOutputStream) {
      out.writeBool(number, value)
    }
    def size(number: Int, value: Boolean) = CodedOutputStream.computeBoolSize(number, value)
  }

  val FloatSerializer = new FieldSerializer[Float] {
    def serialize(number: Int, value: Float, out: CodedOutputStream) {
      out.writeFloat(number, value)
    }
    def size(number: Int, value: Float) = CodedOutputStream.computeFloatSize(number, value)
  }

  val DoubleSerializer = new FieldSerializer[Double] {
    def serialize(number: Int, value: Double, out: CodedOutputStream) {
      out.writeDouble(number, value)
    }
    def size(number: Int, value: Double) = CodedOutputStream.computeDoubleSize(number, value)
  }

  val StringSerializer = new FieldSerializer[String] {
    def serialize(number: Int, value: String, out: CodedOutputStream) {
      out.writeString(number, value)
    }
    def size(number: Int, value: String) = CodedOutputStream.computeStringSize(number, value)
  }

  def optional[T](underlying: FieldSerializer[T]) = new FieldSerializer[Option[T]] {
    def serialize(number: Int, value: Option[T], out: CodedOutputStream) {
      value foreach { v =>
        underlying.serialize(number, v, out)
      }
    }

    def size(number: Int, value: Option[T]) = value match {
      case Some(v) => underlying.size(number, v)
      case None    => 0
    }
  }

  def repeated[T](underlying: FieldSerializer[T]) = new FieldSerializer[Seq[T]] {
    def serialize(number: Int, value: Seq[T], out: CodedOutputStream) {
      value foreach { v =>
        underlying.serialize(number, v, out)
      }
    }

    def size(number: Int, value: Seq[T]) = value.map(underlying.size(number, _)).sum
  }
}

trait MessageSerializier extends FieldSerializer[Any] {
  /**
   * For top-level message serializing
   */
  def serialize(value: Any, out: CodedOutputStream)
  def size(value: Any): Int

  /**
   * For embedded messages
   */
  def serialize(number: Int, value: Any, out: CodedOutputStream) {
    out.writeTag(number, WireFormat.WIRETYPE_LENGTH_DELIMITED)
    out.writeRawVarint32(size(value))
    serialize(value, out)
  }

  def size(number: Int, value: Any) = {
    val s = size(value)
    CodedOutputStream.computeTagSize(number) + CodedOutputStream.computeRawVarint32Size(s) + s
  }
}
