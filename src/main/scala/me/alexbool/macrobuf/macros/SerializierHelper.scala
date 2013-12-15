package me.alexbool.macrobuf.macros

import scala.reflect.macros.WhiteboxContext
import com.google.protobuf.{WireFormat, CodedOutputStream}
import me.alexbool.macrobuf.MessageMetadata

private[macros] class SerializierHelper[C <: WhiteboxContext](val c: C) {

  val mm = MessageMetadata[c.universe.type](c.universe)

  import c.universe._
  import mm._

  private def writePrimitive(tpe: c.Type)(out: c.Expr[CodedOutputStream], number: c.Expr[Int], value: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe.definitions._
    if      (tpe =:= IntTpe)         reify { out.splice.writeInt32(number.splice, value.asInstanceOf[c.Expr[Int]].splice)     }
    else if (tpe =:= LongTpe)        reify { out.splice.writeInt64(number.splice, value.asInstanceOf[c.Expr[Long]].splice)    }
    else if (tpe =:= ShortTpe)       reify { out.splice.writeInt32(number.splice, value.asInstanceOf[c.Expr[Short]].splice)   }
    else if (tpe =:= BooleanTpe)     reify { out.splice.writeBool(number.splice, value.asInstanceOf[c.Expr[Boolean]].splice)  }
    else if (tpe =:= FloatTpe)       reify { out.splice.writeFloat(number.splice, value.asInstanceOf[c.Expr[Float]].splice)   }
    else if (tpe =:= DoubleTpe)      reify { out.splice.writeDouble(number.splice, value.asInstanceOf[c.Expr[Double]].splice) }
    else if (tpe =:= typeOf[String]) reify { out.splice.writeString(number.splice, value.asInstanceOf[c.Expr[String]].splice) }
    else throw new IllegalArgumentException("Unsupported primitive type")
  }

  private def writePrimitiveNoTag(tpe: c.Type)(out: c.Expr[CodedOutputStream], value: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe.definitions._
    if      (tpe =:= IntTpe)         reify { out.splice.writeInt32NoTag(value.asInstanceOf[c.Expr[Int]].splice)     }
    else if (tpe =:= LongTpe)        reify { out.splice.writeInt64NoTag(value.asInstanceOf[c.Expr[Long]].splice)    }
    else if (tpe =:= ShortTpe)       reify { out.splice.writeInt32NoTag(value.asInstanceOf[c.Expr[Short]].splice)   }
    else if (tpe =:= BooleanTpe)     reify { out.splice.writeBoolNoTag(value.asInstanceOf[c.Expr[Boolean]].splice)  }
    else if (tpe =:= FloatTpe)       reify { out.splice.writeFloatNoTag(value.asInstanceOf[c.Expr[Float]].splice)   }
    else if (tpe =:= DoubleTpe)      reify { out.splice.writeDoubleNoTag(value.asInstanceOf[c.Expr[Double]].splice) }
    else if (tpe =:= typeOf[String]) reify { out.splice.writeStringNoTag(value.asInstanceOf[c.Expr[String]].splice) }
    else throw new IllegalArgumentException("Unsupported primitive type")
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
    val readyBody = body(c.Expr(Ident(TermName("msg"))))
    reify {
      for (msg <- collection.splice) {
        readyBody.splice
      }
    }
  }

  private def sizeOfPrimitive(tpe: c.Type)(number: c.Expr[Int], value: c.Expr[Any]): c.Expr[Int] = {
    val sizeNoTag = sizeOfPrimitiveNoTag(tpe)(value)
    reify {
      CodedOutputStream.computeTagSize(number.splice) + sizeNoTag.splice
    }
  }

  private def sizeOfPrimitiveNoTag(tpe: c.Type)(value: c.Expr[Any]): c.Expr[Int] = {
    import c.universe.definitions._
    if      (tpe =:= IntTpe)         reify { CodedOutputStream.computeInt32SizeNoTag(value.asInstanceOf[c.Expr[Int]].splice)     }
    else if (tpe =:= LongTpe)        reify { CodedOutputStream.computeInt64SizeNoTag(value.asInstanceOf[c.Expr[Long]].splice)    }
    else if (tpe =:= ShortTpe)       reify { CodedOutputStream.computeInt32SizeNoTag(value.asInstanceOf[c.Expr[Short]].splice)   }
    else if (tpe =:= BooleanTpe)     reify { CodedOutputStream.computeBoolSizeNoTag(value.asInstanceOf[c.Expr[Boolean]].splice)  }
    else if (tpe =:= FloatTpe)       reify { CodedOutputStream.computeFloatSizeNoTag(value.asInstanceOf[c.Expr[Float]].splice)   }
    else if (tpe =:= DoubleTpe)      reify { CodedOutputStream.computeDoubleSizeNoTag(value.asInstanceOf[c.Expr[Double]].splice) }
    else if (tpe =:= typeOf[String]) reify { CodedOutputStream.computeStringSizeNoTag(value.asInstanceOf[c.Expr[String]].splice) }
    else throw new IllegalArgumentException("Unsupported primitive type")
  }

  private def sizeOfRepeatedPrimitive(tpe: c.Type)(number: c.Expr[Int], value: c.Expr[Iterable[Any]]): c.Expr[Int] = {
    val mapper =
      Function(
        List(ValDef(Modifiers(Flag.PARAM), TermName("m"), Ident(tpe.typeSymbol), EmptyTree)),
        sizeOfPrimitive(tpe)(number, c.Expr(Ident(TermName("m")))).tree
      )
    sizeOfRepeated(value, c.Expr(mapper))
  }

  private def sizeOfPackedRepeatedPrimitive(tpe: c.Type)(value: c.Expr[Iterable[Any]]): c.Expr[Int] = {
    val mapper =
      Function(
        List(ValDef(Modifiers(Flag.PARAM), TermName("m"), Ident(tpe.typeSymbol), EmptyTree)),
        sizeOfPrimitiveNoTag(tpe)(c.Expr(Ident(TermName("m")))).tree
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
    case p: RequiredPrimitive => {
      val exprF: c.Expr[Any] => c.Expr[Unit] = e => writePrimitive(p.actualType)(out, toExpr(p.number), e)
      exprF(fieldValue(obj, f))
    }
    case p: OptionalPrimitive => {
      val exprF: c.Expr[Any] => c.Expr[Unit] = e => writePrimitive(p.actualType)(out, toExpr(p.number), e)
      optional(fieldValue(obj, f).asInstanceOf[c.Expr[Option[Any]]], exprF)
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
      exprF(fieldValue(obj, em))
    }
    case em: OptionalMessage => {
      val exprF: c.Expr[Any] => c.Expr[Unit] = e => c.Expr(Block(serializeEmbeddedMessage(em, e, out).map(_.tree), Literal(Constant(()))))
      optional(fieldValue(obj, f).asInstanceOf[c.Expr[Option[Any]]], exprF)
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
      case f: OptionalPrimitive => {
          val mapper =
            Function(
              List(ValDef(Modifiers(Flag.PARAM), TermName("m"), Ident(f.actualType.typeSymbol), EmptyTree)),
              sizeOfPrimitive(f.actualType)(toExpr(f.number), c.Expr(Ident(TermName("m")))).tree
            )
          val mappedOption = mapOption[Any, Int](fieldValue[Option[Any]](obj, f), c.Expr(mapper))
          reify { mappedOption.splice.getOrElse(0) }
      }
      case f: RequiredPrimitive => sizeOfPrimitive(f.actualType)(toExpr(f.number), fieldValue(obj, f))
      case f: RepeatedPrimitive if !f.packed => {
        sizeOfRepeatedPrimitive(f.actualType)(toExpr(f.number), fieldValue[Iterable[Any]](obj, f))
      }
      case f: RepeatedPrimitive if f.packed => {
        val tagSize = sizeOfTag(toExpr(f.number))
        val valueSize = sizeOfPackedRepeatedPrimitive(f.actualType)(fieldValue[Iterable[Any]](obj, f))
        reify { tagSize.splice + valueSize.splice }
      }
      case f: OptionalMessage => {
          val mapper =
            Function(
              List(ValDef(Modifiers(Flag.PARAM), TermName("m"), Ident(f.actualType.typeSymbol), EmptyTree)),
              messageSizeWithTagAndSize(f, c.Expr(Ident(TermName("m")))).tree
            )
          val mappedOption = mapOption[Any, Int](fieldValue[Option[Any]](obj, f), c.Expr(mapper))
          reify { mappedOption.splice.getOrElse(0) }
      }
      case f: EmbeddedMessage => messageSizeWithTagAndSize(f, fieldValue(obj, f))
      case f: RepeatedMessage => {
        val mapper =
          Function(
            List(ValDef(Modifiers(Flag.PARAM), TermName("m"), Ident(f.actualType.typeSymbol), EmptyTree)),
            messageSizeWithTagAndSize(f, c.Expr(Ident(TermName("m")))).tree
          )
        sizeOfRepeated(fieldValue[Iterable[Any]](obj, f), c.Expr(mapper))
      }
    })
      .reduce((e1, e2) => reify { e1.splice + e2.splice })
  }

  private def messageSizeWithTagAndSize(m: MessageField, obj: c.Expr[Any]): c.Expr[Int] = {
    val tagSize: c.Expr[Int] = sizeOfTag(toExpr(m.number))
    val msgSize: c.Expr[Int] = messageSize(m, obj)
    reify {
      val messageSize = msgSize.splice
      tagSize.splice + messageSize + CodedOutputStream.computeInt32SizeNoTag(messageSize)
    }
  }

  private def mapOption[T, R](option: c.Expr[Option[T]], mapper: c.Expr[T => R]): c.Expr[Option[R]] = reify {
    option.splice.map(mapper.splice)
  }
}
