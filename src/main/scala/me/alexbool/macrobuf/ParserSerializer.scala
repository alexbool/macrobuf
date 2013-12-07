package me.alexbool.macrobuf

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import com.google.protobuf.{CodedInputStream, CodedOutputStream}

trait Serializer[M] {
  protected def doSerialize(obj: M, output: CodedOutputStream)
  protected def size(obj: M): Int

  def serialize(obj: M, output: OutputStream) {
    serialize(obj, CodedOutputStream.newInstance(output))
  }

  def serialize(obj: M, output: CodedOutputStream) {
    doSerialize(obj, output)
    output.flush()
  }

  def serialize(obj: M): Array[Byte] = {
    val output = new ByteArrayOutputStream
    try {
      serialize(obj, output)
      output.toByteArray
    } finally {
      output.close()
    }
  }

  def serializeDelimited(objs: Iterable[M], output: CodedOutputStream) {
    for (obj <- objs) {
      output.writeInt32NoTag(size(obj))
      doSerialize(obj, output)
    }
    output.flush()
  }

  def serializeDelimited(objs: Iterable[M], output: OutputStream) {
    serializeDelimited(objs, CodedOutputStream.newInstance(output))
  }

  def serializeDelimited(objs: Iterable[M]): Array[Byte] = {
    val output = new ByteArrayOutputStream
    try {
      serializeDelimited(objs, output)
      output.toByteArray
    } finally {
      output.close()
    }
  }
}

trait Parser[M] {
  protected def doParseUntilLimit(input: CodedInputStream): M

  def parse(input: CodedInputStream): M = doParseUntilLimit(input)
  def parse(input: InputStream): M = parse(CodedInputStream.newInstance(input))

  def parse(data: Array[Byte]): M = {
    val input = new ByteArrayInputStream(data)
    try {
      parse(input)
    } finally {
      input.close()
    }
  }

  def parseDelimited(input: CodedInputStream): Seq[M] = new ParseLengthDelimitedIterator(input).to[Seq]
  def parseDelimited(input: InputStream): Seq[M] = parseDelimited(CodedInputStream.newInstance(input))

  def parseDelimited(data: Array[Byte]): Seq[M] = {
    val input = new ByteArrayInputStream(data)
    try {
      parseDelimited(input)
    } finally {
      input.close()
    }
  }

  private class ParseLengthDelimitedIterator(in: CodedInputStream) extends Iterator[M] {
    def hasNext = !in.isAtEnd
    def next() = {
      val size = in.readRawVarint32()
      val oldLimit = in.pushLimit(size)
      val result = doParseUntilLimit(in)
      in.popLimit(oldLimit)
      result
    }
  }
}

trait ParserSerializer[M] extends Parser[M] with Serializer[M]
