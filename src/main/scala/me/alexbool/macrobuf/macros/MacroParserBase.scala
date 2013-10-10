package me.alexbool.macrobuf.macros

import me.alexbool.macrobuf.Parser
import java.io.InputStream
import com.google.protobuf.CodedInputStream

abstract class MacroParserBase[T] extends Parser[T] {
  def parse(input: InputStream): T = {
     val codedIn = CodedInputStream.newInstance(input)
     parse(codedIn)
  }

  protected def parse(input: CodedInputStream): T
}

abstract class ListMacroParserBase[T] extends Parser[Seq[T]] {
  def parse(input: InputStream): Seq[T] = {
    val codedIn = CodedInputStream.newInstance(input)
    val buffer = collection.mutable.ListBuffer[T]()
    while (!codedIn.isAtEnd) {
      buffer += parseLengthDelimited(codedIn)
    }
    buffer.to[Seq]
  }

  protected def parseLengthDelimited(input: CodedInputStream): T
}
