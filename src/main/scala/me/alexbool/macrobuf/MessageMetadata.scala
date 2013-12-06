package me.alexbool.macrobuf

import scala.reflect.api.Universe
import me.alexbool.macrobuf.annotation.packed

private[macrobuf] class MessageMetadata[U <: Universe](val u: U) {
  import u._
  import u.definitions._

  type Getter = MethodSymbol

  // Object and subtraits
  sealed trait Object {
    def thisType: Type
    def actualType: Type
  }

  sealed trait Field extends Object {
    def number: Int
    def getter: Getter
    def fieldName: String = getter.name.decoded
    val thisType = getter.returnType
  }

  sealed trait RequiredField extends Field {
    val actualType = thisType
  }

  sealed trait OptionalField extends Field {
    require(getter.returnType <:< typeOf[Option[_]], "Optional field must be subclass of Option[_]")
    val actualType = firstTypeArgument(getter.returnType)
  }

  sealed trait RepeatedField extends Field {
    require(getter.returnType <:< typeOf[Iterable[_]], "Repeated field must be subclass of Iterable[_]")
    val actualType = firstTypeArgument(getter.returnType)
  }

  // MessageOrPrimitive and subtraits
  sealed trait MessageOrPrimitive {
    this: Object =>
  }

  sealed trait Message extends MessageOrPrimitive { this: Object =>
    def fields: Seq[Field]
    def messageName: String = actualType.typeSymbol.name.decoded
    def ctor: MethodSymbol = actualType.declaration(nme.CONSTRUCTOR).asMethod
  }

  sealed trait Primitive extends MessageOrPrimitive {
    this: Object =>
  }

  // Implementation case classes
  case class RequiredPrimitive(number: Int, getter: Getter)                  extends RequiredField with Primitive
  case class OptionalPrimitive(number: Int, getter: Getter)                  extends OptionalField with Primitive
  case class RepeatedPrimitive(number: Int, getter: Getter, packed: Boolean) extends RepeatedField with Primitive {
    if (packed)
      require(isStrictlyPrimitive(this.actualType), "Only primitive type fields can be packed")
  }

  case class EmbeddedMessage(number: Int, getter: Getter, fields: Seq[Field]) extends RequiredField with Message
  case class OptionalMessage(number: Int, getter: Getter, fields: Seq[Field]) extends OptionalField with Message
  case class RepeatedMessage(number: Int, getter: Getter, fields: Seq[Field]) extends RepeatedField with Message

  case class RootMessage(override val messageName: String, fields: Seq[Field], thisType: Type) extends Object with Message {
    val actualType = thisType
  }

  // Type aliases
  type MessageObject = Message with Object
  type MessageField  = Message with Field

  // Factories
  def apply[T](implicit tt: WeakTypeTag[T]): RootMessage = apply(tt.tpe)
  def apply(tpe: Type): RootMessage = RootMessage(tpe.typeSymbol.name.decoded, fieldsFor(tpe), tpe)

  // Utilities
  private def firstTypeArgument(tpe: Type): Type = typeArguments(tpe).head

  private def typeArguments(tpe: Type) = tpe.asInstanceOf[TypeRefApi].args

  private def fieldsFor(tpe: Type): Seq[Field] = {
    val numbers = Stream from 1
    scalaFields(tpe).zip(numbers).map { typeAndNum => fieldFor(typeAndNum._2, typeAndNum._1) }
  }

  private def scalaFields(tpe: Type): Seq[Getter] =
    tpe.members
      .sorted
      .filter(m => m.isPublic && m.isMethod && m.asMethod.isGetter)
      .map(_.asMethod)
      .to[Seq]

  private def fieldFor(number: Int, getter: Getter): Field = {
    val tpe = getter.returnType
    if      (isPrimitive(tpe))            RequiredPrimitive(number, getter)
    else if (tpe <:< typeOf[Option[_]])   if (isPrimitive(firstTypeArgument(tpe))) OptionalPrimitive(number, getter)
    else OptionalMessage(number, getter, fieldsFor(firstTypeArgument(tpe)))
    else if (tpe <:< typeOf[Iterable[_]]) if (isPrimitive(firstTypeArgument(tpe))) RepeatedPrimitive(number, getter, isPacked(getter))
    else RepeatedMessage(number, getter, fieldsFor(firstTypeArgument(tpe)))
    else                                  EmbeddedMessage(number, getter, fieldsFor(tpe))
  }

  private def isPrimitive(tpe: Type): Boolean = isStrictlyPrimitive(tpe) || tpe <:< typeOf[String]
  private def isStrictlyPrimitive(tpe: Type): Boolean = tpe <:< AnyValTpe

  private def isPacked(getter: Getter): Boolean =
    getter.annotations.find(_.tpe =:= typeOf[packed]).map(_.scalaArgs.head match {
      case Literal(Constant(isPacked: Boolean))                  => isPacked
      case Select(_, term) if term.decoded == "<init>$default$1" => true // XXX Bad hack!
      case _                                                     => throw new IllegalArgumentException("Illegal annotation parameter")
    }).getOrElse(false)
}

object MessageMetadata {
  def apply[U <: Universe](u: U) = new MessageMetadata[u.type](u)
  val runtime = MessageMetadata[scala.reflect.runtime.universe.type](scala.reflect.runtime.universe)
}
