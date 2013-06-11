package me.alexbool.macrobuf

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}

trait Serializer[Message] {
  def serialize(obj: Message, output: OutputStream)

  def serialize(obj: Message): Array[Byte] = {
    val output = new ByteArrayOutputStream
    try {
      serialize(obj, output)
      output.toByteArray
    } finally {
      output.close()
    }
  }
}

trait Parser[Message] {
  def parse(input: InputStream): Message

  def parse(data: Array[Byte]): Message = {
    val input = new ByteArrayInputStream(data)
    try {
      parse(input)
    } finally {
      input.close()
    }
  }
}

trait ParserSerializer[T] extends Parser[T] with Serializer[T]
