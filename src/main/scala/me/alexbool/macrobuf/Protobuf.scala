package me.alexbool.macrobuf

import scala.reflect.runtime.universe._
import me.alexbool.macrobuf.macros.Macros
import me.alexbool.macrobuf.reflection.{ReflectionSerializer, ReflectionParser}

object Protobuf {
  import language.experimental.macros

  def serializer[T: TypeTag]: Serializer[T] = new ReflectionSerializer[T](implicitly[TypeTag[T]].tpe)
  def parser[T: TypeTag]: Parser[T] = new ReflectionParser[T](implicitly[TypeTag[T]].tpe)

  def serializerFromClass[T](clazz: Class[T]): Serializer[T] =
    new ReflectionSerializer[T](runtimeMirror(clazz.getClassLoader).classSymbol(clazz).selfType)
  def parserFromClass[T](clazz: Class[T]): Parser[T] =
    new ReflectionParser[T](runtimeMirror(clazz.getClassLoader).classSymbol(clazz).selfType)

  def macroSerializer[T]: Serializer[T] = macro Macros.serializer[T]
  def macroParser[T]: Parser[T] = macro Macros.parser[T]
}
