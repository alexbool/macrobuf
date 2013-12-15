package me.alexbool.macrobuf.reflection

import com.google.protobuf.CodedInputStream
import me.alexbool.macrobuf.util.ParseUntilLimitIterator

trait FieldParser[T] {
  /** Possible several elements at at time, such as packed repeated fields */
  def parse(in: CodedInputStream): Seq[T]
}

trait ScalarFieldParser[T] extends FieldParser[T] {
  def parse(in: CodedInputStream) = Seq(parseOne(in))
  def parseOne(in: CodedInputStream): T
}

object FieldParsers {
  def intParser = new ScalarFieldParser[Int] {
    def parseOne(in: CodedInputStream) = in.readInt32()
  }

  def longParser = new ScalarFieldParser[Long] {
    def parseOne(in: CodedInputStream) = in.readInt64()
  }

  def shortParser = new ScalarFieldParser[Short] {
    def parseOne(in: CodedInputStream) = in.readInt32().toShort
  }

  def booleanParser = new ScalarFieldParser[Boolean] {
    def parseOne(in: CodedInputStream) = in.readBool()
  }

  def floatParser = new ScalarFieldParser[Float] {
    def parseOne(in: CodedInputStream) = in.readFloat()
  }

  def doubleParser = new ScalarFieldParser[Double] {
    def parseOne(in: CodedInputStream) = in.readDouble()
  }

  def stringParser = new ScalarFieldParser[String] {
    def parseOne(in: CodedInputStream) = in.readString()
  }

  def packedRepeated[T](underlying: ScalarFieldParser[T]) = new FieldParser[T] {
    def parse(input: CodedInputStream) = {
      val size = input.readRawVarint32()
      val oldLimit = input.pushLimit(size)
      val result = new ParseUntilLimitIterator[T] {
        protected def in = input
        def next() = underlying.parseOne(input)
      }.to[Seq]
      input.popLimit(oldLimit)
      result
    }
  }
}
