package me.alexbool.macrobuf.macros

import scala.reflect.macros.Context
import com.google.protobuf.{WireFormat, CodedOutputStream}
import me.alexbool.macrobuf.MessageMetadata

private[macros] class SerializierHelper[C <: Context](val c: C) {

  val mm = MessageMetadata[c.universe.type](c.universe)

  import c.universe._
  import mm._

  /**
   * Constructs expression to write given value expression
   */
  private def writePrimitive(tpe: c.Type)(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[Any]): c.Expr[Unit] = {
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
  // XXX Inline all those methods
  private def writeInt(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[Int]): c.Expr[Unit] =
    reify {
      out.splice.writeInt32(number.splice, value.splice)
    }

  private def writeLong(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[Long]): c.Expr[Unit] =
    reify {
      out.splice.writeInt64(number.splice, value.splice)
    }

  private def writeShort(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[Short]): c.Expr[Unit] =
    reify {
      out.splice.writeInt32(number.splice, value.splice)
    }

  private def writeBoolean(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[Boolean]): c.Expr[Unit] =
    reify {
      out.splice.writeBool(number.splice, value.splice)
    }

  private def writeFloat(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[Float]): c.Expr[Unit] =
    reify {
      out.splice.writeFloat(number.splice, value.splice)
    }

  private def writeDouble(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[Double]): c.Expr[Unit] =
    reify {
      out.splice.writeDouble(number.splice, value.splice)
    }

  private def writeString(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[String]): c.Expr[Unit] =
    reify {
      out.splice.writeString(number.splice, value.splice)
    }

  private def writePrimitiveNoTag(tpe: c.Type)(out: c.Expr[CodedOutputStream], value: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe.definitions._
    if      (tpe =:= IntTpe)         writeIntNoTag(out, value.asInstanceOf[c.Expr[Int]])
    else if (tpe =:= LongTpe)        writeLongNoTag(out, value.asInstanceOf[c.Expr[Long]])
    else if (tpe =:= ShortTpe)       writeShortNoTag(out, value.asInstanceOf[c.Expr[Short]])
    else if (tpe =:= BooleanTpe)     writeBooleanNoTag(out, value.asInstanceOf[c.Expr[Boolean]])
    else if (tpe =:= FloatTpe)       writeFloatNoTag(out, value.asInstanceOf[c.Expr[Float]])
    else if (tpe =:= DoubleTpe)      writeDoubleNoTag(out, value.asInstanceOf[c.Expr[Double]])
    else if (tpe =:= typeOf[String]) writeStringNoTag(out, value.asInstanceOf[c.Expr[String]])
    else throw new IllegalArgumentException("Unsupported primitive type")
  }

  // XXX Inline all those methods
  private def writeIntNoTag(out: c.Expr[CodedOutputStream], value: c.Expr[Int]): c.Expr[Unit] = reify {
    out.splice.writeInt32NoTag(value.splice)
  }

  private def writeLongNoTag(out: c.Expr[CodedOutputStream], value: c.Expr[Long]): c.Expr[Unit] = reify {
    out.splice.writeInt64NoTag(value.splice)
  }

  private def writeShortNoTag(out: c.Expr[CodedOutputStream], value: c.Expr[Short]): c.Expr[Unit] = reify {
    out.splice.writeInt32NoTag(value.splice)
  }

  private def writeBooleanNoTag(out: c.Expr[CodedOutputStream], value: c.Expr[Boolean]): c.Expr[Unit] = reify {
    out.splice.writeBoolNoTag(value.splice)
  }

  private def writeFloatNoTag(out: c.Expr[CodedOutputStream], value: c.Expr[Float]): c.Expr[Unit] = reify {
    out.splice.writeFloatNoTag(value.splice)
  }

  private def writeDoubleNoTag(out: c.Expr[CodedOutputStream], value: c.Expr[Double]): c.Expr[Unit] = reify {
    out.splice.writeDoubleNoTag(value.splice)
  }

  private def writeStringNoTag(out: c.Expr[CodedOutputStream], value: c.Expr[String]): c.Expr[Unit] = reify {
    out.splice.writeStringNoTag(value.splice)
  }

  private def optional[T](option: c.Expr[Option[T]], body: c.Expr[T] => c.Expr[Unit]): c.Expr[Unit] = {
    val readyBody = body(reify { option.splice.get })
    reify {
      if (option.splice.isDefined) {
        readyBody.splice
      }
    }
  }

  private def repeated[T](collection: c.Expr[Seq[T]], body: c.Expr[T] => c.Expr[Unit]): c.Expr[Unit] = {
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
  private def sizeOfPrimitive(tpe: c.Type)(number: c.Expr[Int], value: c.Expr[Any]): c.Expr[Int] = {
    val sizeNoTag = sizeOfPrimitiveNoTag(tpe)(value)
    reify {
      CodedOutputStream.computeTagSize(number.splice) + sizeNoTag.splice
    }
  }

  private def sizeOfPrimitiveNoTag(tpe: c.Type)(value: c.Expr[Any]): c.Expr[Int] = {
    import c.universe.definitions._
    if      (tpe =:= IntTpe)         sizeOfInt(value.asInstanceOf[c.Expr[Int]])
    else if (tpe =:= LongTpe)        sizeOfLong(value.asInstanceOf[c.Expr[Long]])
    else if (tpe =:= ShortTpe)       sizeOfShort(value.asInstanceOf[c.Expr[Short]])
    else if (tpe =:= BooleanTpe)     sizeOfBoolean(value.asInstanceOf[c.Expr[Boolean]])
    else if (tpe =:= FloatTpe)       sizeOfFloat(value.asInstanceOf[c.Expr[Float]])
    else if (tpe =:= DoubleTpe)      sizeOfDouble(value.asInstanceOf[c.Expr[Double]])
    else if (tpe =:= typeOf[String]) sizeOfString(value.asInstanceOf[c.Expr[String]])
    else throw new IllegalArgumentException("Unsupported primitive type")
  }

  private def sizeOfRepeatedPrimitive(tpe: c.Type)(number: c.Expr[Int], value: c.Expr[Iterable[Any]]): c.Expr[Int] = {
    val mapper =
      Function(
        List(ValDef(Modifiers(Flag.PARAM), newTermName("m"), Ident(tpe.typeSymbol), EmptyTree)),
        sizeOfPrimitive(tpe)(number, c.Expr(Ident(newTermName("m")))).tree
      )
    sizeOfRepeated(value, c.Expr(mapper))
  }

  private def sizeOfPackedRepeatedPrimitive(tpe: c.Type)(value: c.Expr[Iterable[Any]]): c.Expr[Int] = {
    val mapper =
      Function(
        List(ValDef(Modifiers(Flag.PARAM), newTermName("m"), Ident(tpe.typeSymbol), EmptyTree)),
        sizeOfPrimitiveNoTag(tpe)(c.Expr(Ident(newTermName("m")))).tree
      )
    val sizeNoTag = sizeOfRepeated(value, c.Expr(mapper))
    reify {
      sizeNoTag.splice
    }
  }

  private def sizeOfRepeated[T](value: c.Expr[Iterable[T]], sizeF: c.Expr[T => Int]): c.Expr[Int] = reify {
    value.splice.map(sizeF.splice).sum
  }

  private def sizeOfTag(number: c.Expr[Int]): c.Expr[Int] = reify {
    CodedOutputStream.computeTagSize(number.splice)
  }

  // Size calculators
  private def sizeOfInt(value: c.Expr[Int]): c.Expr[Int] =
    reify {
      CodedOutputStream.computeInt32SizeNoTag(value.splice)
    }

  private def sizeOfLong(value: c.Expr[Long]): c.Expr[Int] =
    reify {
      CodedOutputStream.computeInt64SizeNoTag(value.splice)
    }

  private def sizeOfShort(value: c.Expr[Short]): c.Expr[Int] =
    reify {
      CodedOutputStream.computeInt32SizeNoTag(value.splice)
    }

  private def sizeOfBoolean(value: c.Expr[Boolean]): c.Expr[Int] =
    reify {
      CodedOutputStream.computeBoolSizeNoTag(value.splice)
    }

  private def sizeOfFloat(value: c.Expr[Float]): c.Expr[Int] =
    reify {
      CodedOutputStream.computeFloatSizeNoTag(value.splice)
    }

  private def sizeOfDouble(value: c.Expr[Double]): c.Expr[Int] =
    reify {
      CodedOutputStream.computeDoubleSizeNoTag(value.splice)
    }

  private def sizeOfString(value: c.Expr[String]): c.Expr[Int] =
    reify {
      CodedOutputStream.computeStringSizeNoTag(value.splice)
    }

  // Misc
  private def writeEmbeddedMessageTagAndSize(out: c.Expr[CodedOutputStream], number: c.Expr[Int], size: c.Expr[Int]): List[c.Expr[Unit]] =
    List(reify {
      out.splice.writeTag(number.splice, WireFormat.WIRETYPE_LENGTH_DELIMITED)
    },
    reify {
      out.splice.writeRawVarint32(size.splice)
    })

  private def fieldValue[T](obj: c.Expr[Any], f: Field): c.Expr[T] = c.Expr(Select(obj.tree, f.getter))
  private def toExpr[V](v: V): c.Expr[V] = c.Expr[V](Literal(Constant(v)))

  private def serializeField(obj: c.Expr[Any], f: Field, out: c.Expr[CodedOutputStream]): c.Expr[Unit] = f match {
    case p: Primitive => {
      val exprF: c.Expr[Any] => c.Expr[Unit] = e => writePrimitive(p.actualType)(out, toExpr(p.number), e)
      if (p.optional) optional(fieldValue(obj, f).asInstanceOf[c.Expr[Option[Any]]], exprF)
      else exprF(fieldValue(obj, f))
    }
    case rp: RepeatedPrimitive if !rp.packed => {
      val exprF: c.Expr[Any] => c.Expr[Unit] = e => writePrimitive(rp.actualType)(out, toExpr(rp.number), e)
      repeated(fieldValue(obj, f).asInstanceOf[c.Expr[Seq[Any]]], exprF)
    }
    case rp: RepeatedPrimitive if rp.packed => {
      val writeTag = reify {
        out.splice.writeTag(toExpr(rp.number).splice, WireFormat.WIRETYPE_LENGTH_DELIMITED)
      }
      val writeSize = reify {
        out.splice.writeRawVarint32(sizeOfPackedRepeatedPrimitive(rp.actualType)(fieldValue(obj, f)).splice)
      }
      val exprF: c.Expr[Any] => c.Expr[Unit] = e => writePrimitiveNoTag(rp.actualType)(out, e)
      val writeElements = repeated(fieldValue(obj, f), exprF)
      reify {
        writeTag.splice
        writeSize.splice
        writeElements.splice
      }
    }
    case em: EmbeddedMessage => {
      val exprF: c.Expr[Any] => c.Expr[Unit] = e => c.Expr(Block(serializeEmbeddedMessage(em, e, out).map(_.tree), Literal(Constant(()))))
      if (em.optional) optional(fieldValue(obj, f).asInstanceOf[c.Expr[Option[Any]]], exprF)
      else exprF(fieldValue(obj, em))
    }
    case rm: RepeatedMessage => {
      val exprF: c.Expr[Any] => c.Expr[Unit] = e => c.Expr(Block(serializeEmbeddedMessage(rm, e, out).map(_.tree), Literal(Constant(()))))
      repeated(fieldValue(obj, rm).asInstanceOf[c.Expr[Seq[Any]]], exprF)
    }
  }

  private def serializeEmbeddedMessage(m: MessageField, obj: c.Expr[Any], out: c.Expr[CodedOutputStream]): List[c.Expr[Unit]] = {
    // 1. Write fields
    val writeFields = serializeMessage(m, obj, out)
    // 2. Compute size
    val size = messageSize(m, obj)
    // 3. Write tag and size
    val writeTagAndSize = writeEmbeddedMessageTagAndSize(out, toExpr(m.number), size)
    // 4. Combine
    writeTagAndSize ++ writeFields
  }

  def serializeMessage(m: Message, obj: c.Expr[Any], out: c.Expr[CodedOutputStream]): List[c.Expr[Unit]] = {
    require(m.fields.size > 0, s"Message ${m.messageName} has no fields. Messages must contain at least one field")
    m.fields.map(serializeField(obj, _, out)).to[List]
  }

  def messageSize(m: Message, obj: c.Expr[Any]): c.Expr[Int] = {
    // 1. Get sizes of all the fields
    // 2. Sum them
    m.fields
      .map(f => f match {
      case f: Primitive if f.optional => {
          val mapper =
            Function(
              List(ValDef(Modifiers(Flag.PARAM), newTermName("m"), Ident(f.actualType.typeSymbol), EmptyTree)),
              sizeOfPrimitive(f.actualType)(toExpr(f.number), c.Expr(Ident(newTermName("m")))).tree
            )
          val mappedOption = mapOption[Any, Int](fieldValue[Option[Any]](obj, f), c.Expr(mapper))
          reify { mappedOption.splice.getOrElse(0) }
      }
      case f: Primitive if !f.optional => sizeOfPrimitive(f.actualType)(toExpr(f.number), fieldValue(obj, f))
      case f: RepeatedPrimitive if !f.packed => {
        sizeOfRepeatedPrimitive(f.actualType)(toExpr(f.number), fieldValue[Iterable[Any]](obj, f))
      }
      case f: RepeatedPrimitive if f.packed => {
        val tagSize = sizeOfTag(toExpr(f.number))
        val valueSize = sizeOfPackedRepeatedPrimitive(f.actualType)(fieldValue[Iterable[Any]](obj, f))
        reify { tagSize.splice + valueSize.splice }
      }
      case f: EmbeddedMessage if f.optional => {
          val mapper =
            Function(
              List(ValDef(Modifiers(Flag.PARAM), newTermName("m"), Ident(f.actualType.typeSymbol), EmptyTree)),
              messageSizeWithTag(f, c.Expr(Ident(newTermName("m")))).tree
            )
          val mappedOption = mapOption[Any, Int](fieldValue[Option[Any]](obj, f), c.Expr(mapper))
          reify { mappedOption.splice.getOrElse(0) }
      }
      case f: EmbeddedMessage if !f.optional => messageSizeWithTag(f, fieldValue(obj, f))
      case f: RepeatedMessage => {
        val mapper =
          Function(
            List(ValDef(Modifiers(Flag.PARAM), newTermName("m"), Ident(f.actualType.typeSymbol), EmptyTree)),
            messageSizeWithTag(f, c.Expr(Ident(newTermName("m")))).tree
          )
        sizeOfRepeated(fieldValue[Iterable[Any]](obj, f), c.Expr(mapper))
      }
    })
      .reduce((e1, e2) => reify { e1.splice + e2.splice })
  }

  private def messageSizeWithTag(m: MessageField, obj: c.Expr[Any]): c.Expr[Int] = {
    val tagSize: c.Expr[Int] = sizeOfTag(toExpr(m.number))
    val msgSize: c.Expr[Int] = messageSize(m, obj)
    reify {
      tagSize.splice + msgSize.splice
    }
  }

  private def mapOption[T, R](option: c.Expr[Option[T]], mapper: c.Expr[T => R]): c.Expr[Option[R]] = reify {
    option.splice.map(mapper.splice)
  }
}
