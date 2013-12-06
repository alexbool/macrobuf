package me.alexbool.macrobuf.macros

import me.alexbool.macrobuf.Parser
import com.google.protobuf.CodedInputStream

abstract class ListMacroParserBase[T] extends Parser[Seq[T]] {
  def parse(input: CodedInputStream): Seq[T] = {
    val buffer = collection.mutable.ListBuffer[T]()
    while (!input.isAtEnd) {
      buffer += parseLengthDelimited(input)
    }
    buffer.to[Seq]
  }

  protected def parseLengthDelimited(input: CodedInputStream): T
}
