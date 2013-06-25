package me.alexbool.macrobuf

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}

trait Serializer[M] {
  def serialize(obj: M, output: OutputStream)

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
  def parse(input: InputStream): M

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
