package me.alexbool.macrobuf

import scala.reflect.runtime.universe._
import me.alexbool.macrobuf.macros.Macros

object Protobuf {
  import language.experimental.macros

  def serializer[T: TypeTag]: Serializer[T] = new ReflectionProtobufSerializer[T](implicitly[TypeTag[T]].tpe)
  def serializerForList[T: TypeTag]: Serializer[Iterable[T]] = new ListReflectionProtobufSerializer[T](implicitly[TypeTag[T]].tpe)

  def serializerFromClass[T](clazz: Class[T]): Serializer[T] =
    new ReflectionProtobufSerializer[T](runtimeMirror(clazz.getClassLoader).classSymbol(clazz).selfType)
  def listSerializerFromClass[T](clazz: Class[T]): Serializer[Iterable[T]] =
    new ListReflectionProtobufSerializer[T](runtimeMirror(clazz.getClassLoader).classSymbol(clazz).selfType)

  def macroSerializer[T]: Serializer[T] = macro Macros.serializer[T]
  def macroSerializerForList[T]: Serializer[Iterable[T]] = macro Macros.listSerializer[T]
}
