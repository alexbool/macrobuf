package me.alexbool.macrobuf.macros

import me.alexbool.macrobuf.Parser
import java.io.InputStream
import com.google.protobuf.CodedInputStream

trait MacroParserBase[T] extends Parser[T] {
  def parse(input: InputStream): T = {
     val codedIn = CodedInputStream.newInstance(input)
     parse(codedIn)
  }

  protected def parse(input: CodedInputStream): T
}

trait ListMacroParserBase[T] extends Parser[Seq[T]] {
  def parse(input: InputStream): Seq[T] = {
    val codedIn = CodedInputStream.newInstance(input)
    val buffer = collection.mutable.ListBuffer[T]()
    while (!codedIn.isAtEnd) {
      val size = codedIn.readRawVarint32()
      val oldLimit = codedIn.pushLimit(size)
      buffer += parse(codedIn)
      codedIn.popLimit(oldLimit)
    }
    buffer.to[Seq]
  }

  protected def parse(input: CodedInputStream): T
}
