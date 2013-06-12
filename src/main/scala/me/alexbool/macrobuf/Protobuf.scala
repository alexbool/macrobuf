package me.alexbool.macrobuf

import scala.reflect.runtime.universe._
import me.alexbool.macrobuf.macros.Macros

object Protobuf {
  import language.experimental.macros

  def serializer[T: TypeTag]: Serializer[T] = new ReflectionSerializer[T](implicitly[TypeTag[T]].tpe)
  def listSerializer[T: TypeTag]: Serializer[Iterable[T]] = new ListReflectionSerializer[T](implicitly[TypeTag[T]].tpe)

  def serializerFromClass[T](clazz: Class[T]): Serializer[T] =
    new ReflectionSerializer[T](runtimeMirror(clazz.getClassLoader).classSymbol(clazz).selfType)
  def listSerializerFromClass[T](clazz: Class[T]): Serializer[Iterable[T]] =
    new ListReflectionSerializer[T](runtimeMirror(clazz.getClassLoader).classSymbol(clazz).selfType)

  def macroSerializer[T]: Serializer[T] = macro Macros.serializer[T]
  def listMacroSerializer[T]: Serializer[Iterable[T]] = macro Macros.listSerializer[T]
}
