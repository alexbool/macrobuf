package me.alexbool.macrobuf.benchmark

import me.alexbool.macrobuf.ParserSerializer
import com.google.protobuf.{Message, Parser, CodedInputStream, CodedOutputStream}

class GoogleCompiledParserSerializer[T, M <: Message](
        googleParser: Parser[M],
        typeFactory: M => T,
        messageFactory: T => M) extends ParserSerializer[T]{
  protected def doParseUntilLimit(input: CodedInputStream) = {
    val message: M = googleParser.parseFrom(input)
    require(message.isInitialized)
    typeFactory(message)
  }

  protected def doSerialize(obj: T, output: CodedOutputStream) = {
    val message: M = messageFactory(obj)
    message.writeTo(output)
  }

  protected def size(obj: T) = messageFactory(obj).getSerializedSize
}
