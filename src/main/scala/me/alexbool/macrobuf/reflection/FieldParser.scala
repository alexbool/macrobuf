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
    def parse(in: CodedInputStream) = {
      val size = in.readRawVarint32()
      val oldLimit = in.pushLimit(size)
      val result = new ParseMessageIterator(in, underlying).to[Seq]
      in.popLimit(oldLimit)
      result
    }
  }
}

private class ParseMessageIterator[T](protected override val in: CodedInputStream, p: ScalarFieldParser[T])
  extends ParseUntilLimitIterator[T] {
  def next() = p.parseOne(in)
}
