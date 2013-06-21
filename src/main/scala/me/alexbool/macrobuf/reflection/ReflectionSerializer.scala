package me.alexbool.macrobuf.reflection

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import scala.reflect.ClassTag
import java.io.OutputStream
import com.google.protobuf.CodedOutputStream
import me.alexbool.macrobuf.{MessageMetadata, Serializer}
import MessageMetadata.runtime._

class ReflectionSerializer[T](tpe: Type) extends Serializer[T] {
  private val serializer = new ReflectionMessageSerializer(MessageMetadata.runtime(tpe))

  def serialize(obj: T, output: OutputStream) {
    val codedOut = CodedOutputStream.newInstance(output)
    serializer.serialize(obj, codedOut)
    codedOut.flush()
  }
}

class ListReflectionSerializer[T](tpe: Type) extends Serializer[Iterable[T]] {
  private val serializer = new ReflectionMessageSerializer(MessageMetadata.runtime(tpe))

  def serialize(objs: Iterable[T], output: OutputStream) {
    val codedOut = CodedOutputStream.newInstance(output)
    for (obj <- objs) {
      serializer.serializeValue(obj, codedOut)
      codedOut.flush()
    }
  }
}

private[macrobuf] class ReflectionMessageSerializer(message: Message) extends MessageSerializier {

  private class FieldAndSerializer(val field: Field, val serializer: FieldSerializer[Any])

  private val fieldSerializers: Seq[FieldAndSerializer] =
    message.fields.map(f => new FieldAndSerializer(f, serializerForField(f)))

  private val m = runtimeMirror(getClass.getClassLoader)
  private val ct = ClassTag[Any](m.runtimeClass(message.thisType))

  def serialize(value: Any, out: CodedOutputStream) {
    val values = fieldValues(value)
    fieldSerializers zip values foreach { ctx =>
      ctx._1.serializer.serialize(ctx._1.field.number, ctx._2, out) // XXX Oppa govnocode!
    }
  }

  def valueSize(value: Any) = {
    val values = fieldValues(value)
    fieldSerializers.zip(values).map(fas => fas._1.serializer.size(fas._1.field.number, fas._2)).sum // XXX Oppa govnocode!
  }

  private def serializerForField(f: Field) = (f match {
    case f: Primitive if f.optional        => FieldSerializers.optional(serializerForPrimitive(f.actualType))
    case f: Primitive if !f.optional       => serializerForPrimitive(f.actualType)
    case f: RepeatedPrimitive if  f.packed => FieldSerializers.packedRepeated(serializerForPrimitive(f.actualType))
    case f: RepeatedPrimitive if !f.packed => FieldSerializers.repeated(serializerForPrimitive(f.actualType))
    case f: EmbeddedMessage if f.optional  => FieldSerializers.optional(new ReflectionMessageSerializer(f))
    case f: EmbeddedMessage if !f.optional => new ReflectionMessageSerializer(f)
    case f: RepeatedMessage                => FieldSerializers.repeated(new ReflectionMessageSerializer(f))
  }).asInstanceOf[FieldSerializer[Any]]

  private def serializerForPrimitive(tpe: Type) = {
    val serializer: FieldSerializer[_] =
      if      (tpe =:= IntTpe)         FieldSerializers.IntSerializer
      else if (tpe =:= LongTpe)        FieldSerializers.LongSerializer
      else if (tpe =:= ShortTpe)       FieldSerializers.ShortSerializer
      else if (tpe =:= BooleanTpe)     FieldSerializers.BooleanSerializer
      else if (tpe =:= FloatTpe)       FieldSerializers.FloatSerializer
      else if (tpe =:= DoubleTpe)      FieldSerializers.DoubleSerializer
      else if (tpe =:= typeOf[String]) FieldSerializers.StringSerializer
      else throw new IllegalArgumentException("Unknown primitive type")
    serializer.asInstanceOf[FieldSerializer[Any]]
  }

  private def fieldValues(obj: Any) = {
    val im = m.reflect(obj)(ct)
    message.fields.map(f => im.reflectMethod(f.getter)())
  }
}
