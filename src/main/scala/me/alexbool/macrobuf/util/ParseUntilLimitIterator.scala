package me.alexbool.macrobuf.util

import com.google.protobuf.CodedInputStream

trait ParseUntilLimitIterator[T] extends Iterator[T] {
  protected def in: CodedInputStream
  def hasNext = !in.isAtEnd
}
