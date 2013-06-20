package me.alexbool.macrobuf.reflection

import com.google.protobuf.CodedInputStream

trait FieldParser[T] {
  def parse(in: CodedInputStream): T
}

object FieldParsers {
  val IntParser = new FieldParser[Int] {
    def parse(in: CodedInputStream) = in.readInt32()
  }

  val LongParser = new FieldParser[Long] {
    def parse(in: CodedInputStream) = in.readInt64()
  }

  val ShortParser = new FieldParser[Short] {
    def parse(in: CodedInputStream) = in.readInt32().toShort
  }

  val BooleanParser = new FieldParser[Boolean] {
    def parse(in: CodedInputStream) = in.readBool()
  }

  val FloatParser = new FieldParser[Float] {
    def parse(in: CodedInputStream) = in.readFloat()
  }

  val DoubleParser = new FieldParser[Double] {
    def parse(in: CodedInputStream) = in.readDouble()
  }

  val StringParser = new FieldParser[String] {
    def parse(in: CodedInputStream) = in.readString()
  }
}

trait MessageParser extends FieldParser[Any] {

  /** Sets limit, then parses using parse(CodedInputStream). Use for embedded messages or length-delimited source */
  def parse(in: CodedInputStream): Any = {
    val size = in.readRawVarint32()
    val oldLimit = in.pushLimit(size)
    val result = parseUntilLimit(in)
    in.popLimit(oldLimit)
    result
  }

  /** Reads from stream until limit/EOF. Use for single root message per input stream */
  def parseUntilLimit(in: CodedInputStream): Any
}
