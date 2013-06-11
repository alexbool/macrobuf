package me.alexbool.macrobuf.macros

import scala.reflect.macros.Context
import com.google.protobuf.{WireFormat, CodedOutputStream}
import me.alexbool.macrobuf.MessageMetadata

class Helper[C <: Context](val c: C) {

  import c.universe._

  val mm = MessageMetadata[c.universe.type](c.universe)

  /**
   * Constructs expression to write given value expression
   */
  def writePrimitive(tpe: c.Type)(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe.definitions._
    if      (tpe =:= IntTpe)         writeInt(out, number, value.asInstanceOf[c.Expr[Int]])
    else if (tpe =:= LongTpe)        writeLong(out, number, value.asInstanceOf[c.Expr[Long]])
    else if (tpe =:= ShortTpe)       writeShort(out, number, value.asInstanceOf[c.Expr[Short]])
    else if (tpe =:= BooleanTpe)     writeBoolean(out, number, value.asInstanceOf[c.Expr[Boolean]])
    else if (tpe =:= FloatTpe)       writeFloat(out, number, value.asInstanceOf[c.Expr[Float]])
    else if (tpe =:= DoubleTpe)      writeDouble(out, number, value.asInstanceOf[c.Expr[Double]])
    else if (tpe =:= typeOf[String]) writeString(out, number, value.asInstanceOf[c.Expr[String]])
    else throw new IllegalArgumentException("Unsupported primitive type")
  }

  // Serializiers for various primitive types
  def writeInt(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[Int]): c.Expr[Unit] =
    reify {
      out.splice.writeInt32(number.splice, value.splice)
    }

  def writeLong(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[Long]): c.Expr[Unit] =
    reify {
      out.splice.writeInt64(number.splice, value.splice)
    }

  def writeShort(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[Short]): c.Expr[Unit] =
    reify {
      out.splice.writeInt32(number.splice, value.splice)
    }

  def writeBoolean(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[Boolean]): c.Expr[Unit] =
    reify {
      out.splice.writeBool(number.splice, value.splice)
    }

  def writeFloat(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[Float]): c.Expr[Unit] =
    reify {
      out.splice.writeFloat(number.splice, value.splice)
    }

  def writeDouble(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[Double]): c.Expr[Unit] =
    reify {
      out.splice.writeDouble(number.splice, value.splice)
    }

  def writeString(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[String]): c.Expr[Unit] =
    reify {
      out.splice.writeString(number.splice, value.splice)
    }

  def optional[T](option: c.Expr[Option[T]], body: c.Expr[T] => c.Expr[Unit]): c.Expr[Unit] = {
    val readyBody = body(reify { option.splice.get })
    reify {
      if (option.splice.isDefined) {
        readyBody.splice
      }
    }
  }

  def repeated[T](collection: c.Expr[Seq[T]], body: c.Expr[T] => c.Expr[Unit]): c.Expr[Unit] = {
    val readyBody = body(c.Expr(Ident(newTermName("msg"))))
    reify {
      for (msg <- collection.splice) {
        readyBody.splice
      }
    }
  }

  /**
   * Constructs expression to calculate size of given primitive value field
   */
  def sizeOfPrimitive(tpe: c.Type)(number: c.Expr[Int], value: c.Expr[Any]): c.Expr[Int] = {
    import c.universe.definitions._
    if      (tpe =:= IntTpe)         sizeOfInt(number, value.asInstanceOf[c.Expr[Int]])
    else if (tpe =:= LongTpe)        sizeOfLong(number, value.asInstanceOf[c.Expr[Long]])
    else if (tpe =:= ShortTpe)       sizeOfShort(number, value.asInstanceOf[c.Expr[Short]])
    else if (tpe =:= BooleanTpe)     sizeOfBoolean(number, value.asInstanceOf[c.Expr[Boolean]])
    else if (tpe =:= FloatTpe)       sizeOfFloat(number, value.asInstanceOf[c.Expr[Float]])
    else if (tpe =:= DoubleTpe)      sizeOfDouble(number, value.asInstanceOf[c.Expr[Double]])
    else if (tpe =:= typeOf[String]) sizeOfString(number, value.asInstanceOf[c.Expr[String]])
    else throw new IllegalArgumentException("Unsupported primitive type")
  }

  def sizeOfRepeatedPrimitive(tpe: c.Type)(number: c.Expr[Int], value: c.Expr[Iterable[Any]]): c.Expr[Int] = {
    val mapper = Function(List(ValDef(Modifiers(Flag.PARAM), newTermName("m"), Ident(tpe.typeSymbol), EmptyTree)),
      sizeOfPrimitive(tpe)(number, c.Expr[tpe.type](Ident(newTermName("m")))).tree)
    sizeOfRepeated(value, c.Expr(mapper))
  }

  def sizeOfRepeated[T](value: c.Expr[Iterable[T]], sizeF: c.Expr[T => Int]): c.Expr[Int] =
    reify {
      value.splice.map(sizeF.splice).sum
    }

  def sizeOfTag(number: c.Expr[Int]): c.Expr[Int] = reify {
    CodedOutputStream.computeTagSize(number.splice)
  }

  // Size calculators
  def sizeOfInt(number: c.Expr[Int], value: c.Expr[Int]): c.Expr[Int] =
    reify {
      CodedOutputStream.computeInt32Size(number.splice, value.splice)
    }

  def sizeOfLong(number: c.Expr[Int], value: c.Expr[Long]): c.Expr[Int] =
    reify {
      CodedOutputStream.computeInt64Size(number.splice, value.splice)
    }

  def sizeOfShort(number: c.Expr[Int], value: c.Expr[Short]): c.Expr[Int] =
    reify {
      CodedOutputStream.computeInt32Size(number.splice, value.splice)
    }

  def sizeOfBoolean(number: c.Expr[Int], value: c.Expr[Boolean]): c.Expr[Int] =
    reify {
      CodedOutputStream.computeBoolSize(number.splice, value.splice)
    }

  def sizeOfFloat(number: c.Expr[Int], value: c.Expr[Float]): c.Expr[Int] =
    reify {
      CodedOutputStream.computeFloatSize(number.splice, value.splice)
    }

  def sizeOfDouble(number: c.Expr[Int], value: c.Expr[Double]): c.Expr[Int] =
    reify {
      CodedOutputStream.computeDoubleSize(number.splice, value.splice)
    }

  def sizeOfString(number: c.Expr[Int], value: c.Expr[String]): c.Expr[Int] =
    reify {
      CodedOutputStream.computeStringSize(number.splice, value.splice)
    }

  // Misc
  def writeEmbeddedMessageTagAndSize(out: c.Expr[CodedOutputStream], number: c.Expr[Int], size: c.Expr[Int]): c.Expr[Unit] =
    reify {
      out.splice.writeTag(number.splice, WireFormat.WIRETYPE_LENGTH_DELIMITED)
      out.splice.writeRawVarint32(size.splice)
    }

  def value(obj: c.Expr[Any], f: mm.Field): c.Expr[Any] = c.Expr(c.universe.Select(obj.tree, f.getter))
  def toExpr[V](v: V): c.Expr[V] = c.Expr[V](Literal(Constant(v)))

  def serializeField(obj: c.Expr[Any], f: mm.Field, out: c.Expr[CodedOutputStream]): c.Expr[Unit] = f match {
    case p: mm.Primitive => {
      val exprF: c.Expr[Any] => c.Expr[Unit] = e => writePrimitive(p.actualType)(out, toExpr(p.number), e)
      if (p.optional) optional(value(obj, f).asInstanceOf[c.Expr[Option[Any]]], exprF)
      else exprF(value(obj, f))
    }
    case rp: mm.RepeatedPrimitive => {
      val exprF: c.Expr[Any] => c.Expr[Unit] = e => writePrimitive(rp.actualType)(out, toExpr(rp.number), e)
      repeated(value(obj, f).asInstanceOf[c.Expr[Seq[Any]]], exprF)
    }
    case em: mm.EmbeddedMessage => {
      val exprF: c.Expr[Any] => c.Expr[Unit] = e => serializeEmbeddedMessage(em, e, out)
      if (em.optional) optional(value(obj, f).asInstanceOf[c.Expr[Option[Any]]], exprF)
      else exprF(value(obj, em))
    }
    case rm: mm.RepeatedMessage => {
      val exprF: c.Expr[Any] => c.Expr[Unit] = e => serializeEmbeddedMessage(rm, e, out)
      repeated(value(obj, rm).asInstanceOf[c.Expr[Seq[Any]]], exprF)
    }
  }

  def serializeEmbeddedMessage(m: mm.MessageField, obj: c.Expr[Any], out: c.Expr[CodedOutputStream]): c.Expr[Unit] = {
    // 1. Write fields
    val writeFields = serializeMessage(m, obj, out)
    // 2. Compute size
    val size = messageSize(m, obj)
    // 3. Write tag and size
    val writeTagAndSize = writeEmbeddedMessageTagAndSize(out, toExpr(m.number), size)
    // 4. Combine
    reify { writeTagAndSize.splice; writeFields.splice }
  }

  def serializeMessage(m: mm.Message, obj: c.Expr[Any], out: c.Expr[CodedOutputStream]): c.Expr[Unit] = {
    require(m.fields.size > 0, s"Message ${m.messageName} has no fields. Messages must contain at least one field")
    m.fields.map(serializeField(obj, _, out)).reduce((f1, f2) => reify { f1.splice; f2.splice })
  }

  def messageSize(m: mm.Message, obj: c.Expr[Any]): c.Expr[Int] = {
    // 1. Get sizes of all the fields
    // 2. Sum them
    m.fields
      .map(f => f match {
      case f: mm.Primitive => {
        if (f.optional) {
          val mapper = Function(List(ValDef(Modifiers(Flag.PARAM), newTermName("m"), Ident(f.actualType.typeSymbol), EmptyTree)),
            sizeOfPrimitive(f.actualType)(toExpr(f.number), c.Expr[f.actualType.type](Ident(newTermName("m")))).tree)
          val mappedOption = mapOption[f.actualType.type, Int](value(obj, f).asInstanceOf[c.Expr[Option[f.actualType.type]]], c.Expr(mapper))
          reify { mappedOption.splice.getOrElse(0) }
        }
        else sizeOfPrimitive(f.actualType)(toExpr(f.number), value(obj, f))
      }
      case f: mm.RepeatedPrimitive => sizeOfRepeatedPrimitive(f.actualType)(toExpr(f.number), value(obj, f).asInstanceOf[c.Expr[Iterable[Any]]])
      case f: mm.EmbeddedMessage => {
        if (f.optional) {
          val mapper = Function(List(ValDef(Modifiers(Flag.PARAM), newTermName("m"), Ident(f.actualType.typeSymbol), EmptyTree)),
            messageSizeWithTag(f, c.Expr[f.actualType.type](Ident(newTermName("m")))).tree)
          val mappedOption = mapOption[f.actualType.type, Int](value(obj, f).asInstanceOf[c.Expr[Option[f.actualType.type]]], c.Expr(mapper))
          reify { mappedOption.splice.getOrElse(0) }
        }
        else messageSizeWithTag(f, value(obj, f))
      }
      case f: mm.RepeatedMessage => {
        val mapper = Function(List(ValDef(Modifiers(Flag.PARAM), newTermName("m"), Ident(f.actualType.typeSymbol), EmptyTree)),
          messageSizeWithTag(f, c.Expr[f.thisType.type](Ident(newTermName("m")))).tree)
        sizeOfRepeated(value(obj, f).asInstanceOf[c.Expr[Iterable[f.thisType.type]]], c.Expr(mapper))
      }
    })
      .reduce((e1, e2) => reify { e1.splice + e2.splice })
  }

  def messageSizeWithTag(m: mm.MessageField, obj: c.Expr[Any]): c.Expr[Int] = {
    val tagSize: c.Expr[Int] = sizeOfTag(toExpr(m.number))
    val msgSize: c.Expr[Int] = messageSize(m, obj)
    reify {
      tagSize.splice + msgSize.splice
    }
  }

  def mapOption[T, R](option: c.Expr[Option[T]], mapper: c.Expr[T => R]): c.Expr[Option[R]] = reify {
    option.splice.map(mapper.splice)
  }
}
