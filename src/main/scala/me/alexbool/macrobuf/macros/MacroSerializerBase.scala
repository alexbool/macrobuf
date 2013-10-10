package me.alexbool.macrobuf.macros

import me.alexbool.macrobuf.Serializer
import java.io.OutputStream
import com.google.protobuf.CodedOutputStream

abstract class MacroSerializerBase[T] extends Serializer[T] {
  def serialize(obj: T, output: OutputStream) {
    val cos = CodedOutputStream.newInstance(output)
    serialize(obj, cos)
    cos.flush()
  }

  protected def serialize(obj: T, output: CodedOutputStream)
}

abstract class ListMacroSerializerBase[T] extends Serializer[Iterable[T]] {
  def serialize(objs: Iterable[T], output: OutputStream) {
    val cos = CodedOutputStream.newInstance(output)
    for (obj <- objs) {
      cos.writeRawVarint32(size(obj))
      serialize(obj, cos)
    }
    cos.flush()
  }

  protected def serialize(obj: T, output: CodedOutputStream)
  protected def size(obj: T): Int
}
