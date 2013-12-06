package me.alexbool.macrobuf.macros

import me.alexbool.macrobuf.Serializer
import com.google.protobuf.CodedOutputStream

abstract class ListMacroSerializerBase[T] extends Serializer[Iterable[T]] {
  def serialize(objs: Iterable[T], output: CodedOutputStream) {
    for (obj <- objs) {
      output.writeRawVarint32(size(obj))
      doSerialize(obj, output)
    }
    output.flush()
  }

  protected def doSerialize(obj: T, output: CodedOutputStream)
  protected def size(obj: T): Int
}
