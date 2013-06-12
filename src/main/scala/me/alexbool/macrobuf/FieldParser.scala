package me.alexbool.macrobuf

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
    def parse(in: CodedInputStream) = in.readInt32().asInstanceOf[Short]
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
