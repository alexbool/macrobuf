package me.alexbool.macrobuf.reflection

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import scala.reflect.ClassTag
import com.google.protobuf.{CodedOutputStream, WireFormat}
import me.alexbool.macrobuf.{MessageMetadata, Serializer}
import MessageMetadata.runtime._

class ReflectionSerializer[T](tpe: Type) extends Serializer[T] {
  private val serializer = new ReflectionMessageSerializer(MessageMetadata.runtime(tpe))

  protected def doSerialize(obj: T, output: CodedOutputStream) {
    serializer.serialize(obj, output)
  }

  protected def size(obj: T) = serializer.valueSize(obj)
}

private[macrobuf] class ReflectionMessageSerializer(message: MessageObject) extends FieldSerializer[Any] {

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

  def serializeTag(number: Int, out: CodedOutputStream) {
    out.writeTag(number, WireFormat.WIRETYPE_LENGTH_DELIMITED)
  }

  def serializeValue(value: Any, out: CodedOutputStream) {
    out.writeRawVarint32(valueSize(value))
    serialize(value, out)
  }

  override def size(number: Int, value: Any) = {
    val sizeOfValue = valueSize(value)
    tagSize(number) + sizeOfValue + CodedOutputStream.computeInt32SizeNoTag(sizeOfValue)
  }

  private def serializerForField(f: Field) = (f match {
    case f: OptionalPrimitive              => FieldSerializers.optional(serializerForPrimitive(f.actualType))
    case f: RequiredPrimitive              => serializerForPrimitive(f.actualType)
    case f: RepeatedPrimitive if  f.packed => FieldSerializers.packedRepeated(serializerForPrimitive(f.actualType))
    case f: RepeatedPrimitive if !f.packed => FieldSerializers.repeated(serializerForPrimitive(f.actualType))
    case f: OptionalMessage                => FieldSerializers.optional(new ReflectionMessageSerializer(f))
    case f: EmbeddedMessage                => new ReflectionMessageSerializer(f)
    case f: RepeatedMessage                => FieldSerializers.repeated(new ReflectionMessageSerializer(f))
  }).asInstanceOf[FieldSerializer[Any]]

  private def serializerForPrimitive(tpe: Type) = {
    val serializer: FieldSerializer[_] =
      if      (tpe =:= IntTpe)         FieldSerializers.intSerializer
      else if (tpe =:= LongTpe)        FieldSerializers.longSerializer
      else if (tpe =:= ShortTpe)       FieldSerializers.shortSerializer
      else if (tpe =:= BooleanTpe)     FieldSerializers.booleanSerializer
      else if (tpe =:= FloatTpe)       FieldSerializers.floatSerializer
      else if (tpe =:= DoubleTpe)      FieldSerializers.doubleSerializer
      else if (tpe =:= typeOf[String]) FieldSerializers.stringSerializer
      else throw new IllegalArgumentException("Unknown primitive type")
    serializer.asInstanceOf[FieldSerializer[Any]]
  }

  private def fieldValues(obj: Any) = {
    val im = m.reflect(obj)(ct)
    message.fields.map(f => im.reflectMethod(f.getter)())
  }
}
