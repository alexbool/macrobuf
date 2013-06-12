package me.alexbool.macrobuf

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import scala.reflect.ClassTag
import java.io.OutputStream
import com.google.protobuf.CodedOutputStream
import MessageMetadata.runtime._

class ReflectionProtobufSerializer[T](tpe: Type) extends Serializer[T] {
  private val serializer = new ReflectionMessageSerializer(MessageMetadata.runtime(tpe))

  def serialize(obj: T, output: OutputStream) {
    val codedOut = CodedOutputStream.newInstance(output)
    serializer.serialize(obj, codedOut)
    codedOut.flush()
  }
}

class ReflectionMessageSerializer(message: Message) extends MessageSerializier {

  private case class FieldAndSerializer(field: Field, serializer: FieldSerializer[Any])

  private val fieldSerializers: Seq[FieldAndSerializer] =
    message.fields.map(f => FieldAndSerializer(f, serializerForField(f)))

  private val m = runtimeMirror(getClass.getClassLoader)

  def serialize(value: Any, out: CodedOutputStream) {
    val values = fieldValues(value)
    fieldSerializers zip values foreach { ctx =>
      ctx._1.serializer.serialize(ctx._1.field.number, ctx._2, out) // XXX Oppa govnocode!
    }
  }

  def size(value: Any) = {
    val values = fieldValues(value)
    fieldSerializers.zip(values).map(fas => fas._1.serializer.size(fas._1.field.number, fas._2)).sum // XXX Oppa govnocode!
  }

  private def serializerForField(f: Field) = (f match {
    case f: Primitive if f.optional        => FieldSerializers.optional(serializerForPrimitive(f.actualType))
    case f: Primitive if !f.optional       => serializerForPrimitive(f.actualType)
    case f: RepeatedPrimitive              => FieldSerializers.repeated(serializerForPrimitive(f.actualType))
    case f: EmbeddedMessage if f.optional  => FieldSerializers.optional(new ReflectionMessageSerializer(f))
    case f: EmbeddedMessage if !f.optional => new ReflectionMessageSerializer(f)
    case f: RepeatedMessage                => FieldSerializers.repeated(new ReflectionMessageSerializer(f))
  }).asInstanceOf[FieldSerializer[Any]]

  private def serializerForPrimitive(tpe: Type) = (tpe match {
    case IntTpe                      => FieldSerializers.IntSerializer
    case LongTpe                     => FieldSerializers.LongSerializer
    case ShortTpe                    => FieldSerializers.ShortSerializer
    case BooleanTpe                  => FieldSerializers.BooleanSerializer
    case FloatTpe                    => FieldSerializers.FloatSerializer
    case DoubleTpe                   => FieldSerializers.DoubleSerializer
    case _ if tpe =:= typeOf[String] => FieldSerializers.StringSerializer
  }).asInstanceOf[FieldSerializer[Any]]

  private def fieldValues(obj: Any) = {
    val im = m.reflect(obj)(ClassTag(m.runtimeClass(message.thisType)))
    message.fields.map(f => im.reflectMethod(f.getter)())
  }
}

class ListReflectionProtobufSerializer[T](tpe: Type) extends Serializer[Iterable[T]] {
  private val serializer = new ReflectionMessageSerializer(MessageMetadata.runtime(tpe))

  def serialize(objs: Iterable[T], output: OutputStream) {
    val codedOut = CodedOutputStream.newInstance(output)
    for (obj <- objs) {
      codedOut.writeRawVarint32(serializer.size(obj))
      serializer.serialize(obj, codedOut)
      codedOut.flush()
    }
  }
}
