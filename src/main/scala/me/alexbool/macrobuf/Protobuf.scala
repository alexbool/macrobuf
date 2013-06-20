package me.alexbool.macrobuf

import scala.reflect.runtime.universe._
import me.alexbool.macrobuf.macros.Macros
import me.alexbool.macrobuf.reflection.{ListReflectionSerializer, ReflectionSerializer, ListReflectionParser, ReflectionParser}

object Protobuf {
  import language.experimental.macros

  def serializer[T: TypeTag]: Serializer[T] = new ReflectionSerializer[T](implicitly[TypeTag[T]].tpe)
  def listSerializer[T: TypeTag]: Serializer[Iterable[T]] = new ListReflectionSerializer[T](implicitly[TypeTag[T]].tpe)

  def parser[T: TypeTag]: Parser[T] = new ReflectionParser[T](implicitly[TypeTag[T]].tpe)
  def listParser[T: TypeTag]: Parser[Seq[T]] = new ListReflectionParser[T](implicitly[TypeTag[T]].tpe)

  def serializerFromClass[T](clazz: Class[T]): Serializer[T] =
    new ReflectionSerializer[T](runtimeMirror(clazz.getClassLoader).classSymbol(clazz).selfType)
  def listSerializerFromClass[T](clazz: Class[T]): Serializer[Iterable[T]] =
    new ListReflectionSerializer[T](runtimeMirror(clazz.getClassLoader).classSymbol(clazz).selfType)

  def parserFromClass[T](clazz: Class[T]): Parser[T] =
    new ReflectionParser[T](runtimeMirror(clazz.getClassLoader).classSymbol(clazz).selfType)
  def listParserFromClass[T](clazz: Class[T]): Parser[Seq[T]] =
    new ListReflectionParser[T](runtimeMirror(clazz.getClassLoader).classSymbol(clazz).selfType)

  def macroSerializer[T]: Serializer[T] = macro Macros.serializer[T]
  def listMacroSerializer[T]: Serializer[Iterable[T]] = macro Macros.listSerializer[T]

  def macroParser[T]: Parser[T] = macro Macros.parser[T]
  def listMacroParser[T]: Parser[Seq[T]] = macro Macros.listParser[T]
}
