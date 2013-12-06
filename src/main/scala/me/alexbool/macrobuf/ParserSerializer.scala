package me.alexbool.macrobuf

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import com.google.protobuf.{CodedInputStream, CodedOutputStream}

trait Serializer[M] {
  def serialize(obj: M, output: CodedOutputStream)

  def serialize(obj: M, output: OutputStream) {
    serialize(obj, CodedOutputStream.newInstance(output))
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
}

trait Parser[M] {
  def parse(input: CodedInputStream): M

  def parse(input: InputStream): M = parse(CodedInputStream.newInstance(input))

  def parse(data: Array[Byte]): M = {
    val input = new ByteArrayInputStream(data)
    try {
      parse(input)
    } finally {
      input.close()
    }
  }
}

trait ParserSerializer[M] extends Parser[M] with Serializer[M]
