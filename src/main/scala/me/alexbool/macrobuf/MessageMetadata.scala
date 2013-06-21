package me.alexbool.macrobuf

import scala.reflect.api.Universe

class MessageMetadata[U <: Universe](val u: U) {
  import u._
  import u.definitions._

  type Getter = MethodSymbol

  sealed trait Object {
    def thisType: Type
    def actualType: Type = thisType
  }

  sealed trait Message extends Object {
    def fields: Seq[Field]
    def messageName: String
    def ctor: MethodSymbol = actualType.declaration(nme.CONSTRUCTOR).asMethod
  }

  case class RootMessage private[MessageMetadata] (messageName: String, fields: Seq[Field], thisType: Type) extends Message

  sealed trait Field extends Object {
    def number: Int
    def getter: Getter
    def fieldName: String = getter.name.decoded
    val thisType = getter.returnType
    override val actualType = {
      val tpe = getter.returnType
      if (tpe <:< typeOf[Option[_]] || tpe <:< typeOf[Iterable[_]]) firstTypeArgument(tpe)
      else tpe
    }
  }

  sealed trait Scalar extends Field {
    def optional: Boolean
  }

  sealed trait MessageField extends Field with Message {
    def messageName = actualType.typeSymbol.name.decoded
  }

  case class Primitive private[macrobuf] (number: Int, getter: Getter, optional: Boolean = false) extends Scalar
  case class EmbeddedMessage private[macrobuf] (number: Int, getter: Getter, fields: Seq[Field], optional: Boolean = false) extends Scalar with MessageField

  sealed trait Repeated extends Field
  case class RepeatedPrimitive private[macrobuf] (number: Int, getter: Getter, packed: Boolean = false) extends Repeated {
    if (packed) require (isStrictlyPrimitive(this.actualType), "Only primitive type fields can be packed")
  }
  case class RepeatedMessage private[macrobuf] (number: Int, getter: Getter, fields: Seq[Field]) extends Repeated with MessageField

  def apply[T](implicit tt: WeakTypeTag[T]): RootMessage = apply(tt.tpe)
  def apply(tpe: Type): RootMessage = RootMessage(tpe.typeSymbol.name.decoded, fieldsFor(tpe), tpe)

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
    if      (isPrimitive(tpe))            Primitive(number, getter, optional = false)
    else if (tpe <:< typeOf[Option[_]])   if (isPrimitive(firstTypeArgument(tpe))) Primitive(number, getter, optional = true)
                                          else EmbeddedMessage(number, getter, fieldsFor(firstTypeArgument(tpe)), optional = true)
    else if (tpe <:< typeOf[Iterable[_]]) if (isPrimitive(firstTypeArgument(tpe))) RepeatedPrimitive(number, getter)
                                          else RepeatedMessage(number, getter, fieldsFor(firstTypeArgument(tpe)))
    else                                  EmbeddedMessage(number, getter, fieldsFor(tpe), optional = false)
  }

  private def isPrimitive(tpe: Type): Boolean = isStrictlyPrimitive(tpe) || tpe <:< typeOf[String]
  private def isStrictlyPrimitive(tpe: Type): Boolean = tpe <:< AnyValTpe

  private def typeArguments(tpe: Type) = tpe match {
    case TypeRef(_, _, args) => args
  }

  private def firstTypeArgument(tpe: Type): Type = typeArguments(tpe).head
}

object MessageMetadata {
  def apply[U <: Universe](u: U) = new MessageMetadata[u.type](u)
  val runtime = MessageMetadata[scala.reflect.runtime.universe.type](scala.reflect.runtime.universe)
}
