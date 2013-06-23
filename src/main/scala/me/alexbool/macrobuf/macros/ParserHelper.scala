package me.alexbool.macrobuf.macros

import scala.reflect.macros.Context
import me.alexbool.macrobuf.MessageMetadata
import com.google.protobuf.{WireFormat, CodedInputStream}

private[macros] class ParserHelper[C <: Context](val c: C) {

  val mm = MessageMetadata[c.universe.type](c.universe)

  import c.universe._
  import mm._
  import WireFormat._

  private def parsePrimitive(tpe: c.Type, in: c.Expr[CodedInputStream]): c.Expr[Any] = {
    import c.universe.definitions._
    if      (tpe =:= IntTpe)         readInt(in)
    else if (tpe =:= LongTpe)        readLong(in)
    else if (tpe =:= ShortTpe)       readShort(in)
    else if (tpe =:= BooleanTpe)     readBoolean(in)
    else if (tpe =:= FloatTpe)       readFloat(in)
    else if (tpe =:= DoubleTpe)      readDouble(in)
    else if (tpe =:= typeOf[String]) readString(in)
    else throw new IllegalArgumentException("Unsupported primitive type")
  }

  private def parsePrimitiveAndRequireWireFormat(tpe: c.Type, tag: c.Expr[Int], in: c.Expr[CodedInputStream]): c.Expr[Any] = {
    val requireWireTypeExpr = requireWireTypeForPrimitive(tpe, tag)
    val parseExpr = parsePrimitive(tpe, in)
    // XXX Flatten 'require' block of exprs
    reify { requireWireTypeExpr.splice; parseExpr.splice }
  }

  // Parsers for primitive types
  private def readInt(in: c.Expr[CodedInputStream]): c.Expr[Int]         = reify { in.splice.readInt32() }
  private def readLong(in: c.Expr[CodedInputStream]): c.Expr[Long]       = reify { in.splice.readInt64() }
  private def readShort(in: c.Expr[CodedInputStream]): c.Expr[Short]     = reify { in.splice.readInt32().toShort }
  private def readBoolean(in: c.Expr[CodedInputStream]): c.Expr[Boolean] = reify { in.splice.readBool() }
  private def readFloat(in: c.Expr[CodedInputStream]): c.Expr[Float]     = reify { in.splice.readFloat() }
  private def readDouble(in: c.Expr[CodedInputStream]): c.Expr[Double]   = reify { in.splice.readDouble() }
  private def readString(in: c.Expr[CodedInputStream]): c.Expr[String]   = reify { in.splice.readString() }

  private def requireWireFormat(tag: c.Expr[Int], wf: Int): c.Expr[Unit] = {
    val wfExpr = c.Expr[Int](Literal(Constant(wf)))
    reify {
      val actualWf = tag.splice & (1 << 3) - 1 // XXX Kinda hack
      require(actualWf == wfExpr.splice, s"Wrong field wire format: found $actualWf, expected ${wfExpr.splice}")
    }
  }

  private def requireWireTypeForPrimitive(tpe: c.Type, tag: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe.definitions._
    if      (tpe =:= IntTpe)         requireWireFormat(tag, WIRETYPE_VARINT)
    else if (tpe =:= LongTpe)        requireWireFormat(tag, WIRETYPE_VARINT)
    else if (tpe =:= ShortTpe)       requireWireFormat(tag, WIRETYPE_VARINT)
    else if (tpe =:= BooleanTpe)     requireWireFormat(tag, WIRETYPE_VARINT)
    else if (tpe =:= FloatTpe)       requireWireFormat(tag, WIRETYPE_FIXED32)
    else if (tpe =:= DoubleTpe)      requireWireFormat(tag, WIRETYPE_FIXED64)
    else if (tpe =:= typeOf[String]) requireWireFormat(tag, WIRETYPE_LENGTH_DELIMITED)
    else throw new IllegalArgumentException("Unsupported primitive type")
  }

  private def parseField(f: Field, tag: c.Expr[Int], in: c.Expr[CodedInputStream]): c.Expr[Any] = f match {
    case f: RepeatedPrimitive if f.packed    => parsePackedRepeatedField(f, tag, in)
    case _: Primitive | _: RepeatedPrimitive => parsePrimitiveAndRequireWireFormat(f.actualType, tag, in)
    case m: MessageField                     => parseDelimited(m, in)
  }

  def parseMessage[T](m: Message, in: c.Expr[CodedInputStream]): c.Expr[T] = {
    // 1. Declare vars
    // 2. Loop while !in.isAtEnd(): read tag, decode field number and type
    // 3. Pattern match on number
    // 4. Require wire type matches field metadata, throw exception if not
    // 5. Read value from loop and assign it to the matching var declared at step 1
    // 6. Go to step 2
    // 7. Check that all required fields are provided
    // 8. Construct message
    // 9. PROFIT!!!
    val varDefs = declareVars(m)
    val varDefsStmts = varDefs.values.to[List]
    val tag = c.Expr[Int](Ident(newTermName("tag")))
    val matchOnNumber = c.Expr[Unit](patternMatchOnFieldNumber(varDefs, tag, in))
    val checkAllRequiredFieldsAreProvidedStmts = checkAllRequiredFieldsAreProvided(varDefs)
    val constructMessageExpr = constructMessage[T](m, varDefs)
    val loopTree = reify {
      while (!in.splice.isAtEnd) {
        val tag = in.splice.readTag()
        val number = WireFormat.getTagFieldNumber(tag)
        matchOnNumber.splice
      }
    }.tree
    c.Expr(Block((varDefsStmts :+ loopTree) ++ checkAllRequiredFieldsAreProvidedStmts, constructMessageExpr.tree))
  }

  def parseDelimited[T](m: Message, in: c.Expr[CodedInputStream]): c.Expr[T] = {
    val parseMessageExpr = parseMessage[T](m, in)
    reify {
      val size = in.splice.readRawVarint32()
      val oldLimit = in.splice.pushLimit(size)
      val result = parseMessageExpr.splice
      in.splice.popLimit(oldLimit)
      result
    }
  }

  private def parsePackedRepeatedField(f: RepeatedPrimitive, tag: c.Expr[Int], in: c.Expr[CodedInputStream]): c.Expr[Seq[Any]] = {
    val tree = reify {
      requireWireFormat(tag, WIRETYPE_LENGTH_DELIMITED).splice
      val length = in.splice.readRawVarint32()
      val oldLimit = in.splice.pushLimit(length)
      val result = new Iterator[Any] {
        def hasNext = !in.splice.isAtEnd
        def next() = parsePrimitive(f.actualType, in).splice
      }.to[Seq]
      in.splice.popLimit(oldLimit)
      result
    }.tree
    // XXX Hack! Wait for quasiquotes and just write q"new Iterator[${f.actualType}] { ... }"
    c.Expr(
      TypeApply(
        Select(tree, newTermName("asInstanceOf")),
        List(
          AppliedTypeTree(
            Ident(newTypeName("Seq")),
            List(TypeTree(f.actualType)
            )
          )
        )
      )
    )
  }

  private def declareVars(m: Message): Map[Field, ValDef] = {
    def varDefForField(f: Field): ValDef = {
      val tpt: Tree = f match {
        case f: Scalar   => AppliedTypeTree(Ident(newTypeName("Option")), List(TypeTree(f.actualType)))
        case f: Repeated => AppliedTypeTree(Ident(newTypeName("Seq")), List(TypeTree(f.actualType)))
      }
      val rhs: Tree = f match {
        case f: Scalar   => reify { None  }.tree
        case f: Repeated => reify { Seq() }.tree
      }
      ValDef(Modifiers(Flag.MUTABLE), newTermName(s"${f.fieldName}${f.number}"), tpt, rhs)
    }
    m.fields.map(f => (f, varDefForField(f))).toMap
  }

  private def patternMatchOnFieldNumber(varDefs: Map[Field, ValDef], tag: c.Expr[Int], in: c.Expr[CodedInputStream]): Match = {
    val cases: List[CaseDef] = varDefs.to[List].map(fieldAndVar => {
      CaseDef(Literal(Constant(fieldAndVar._1.number)), EmptyTree, readField(fieldAndVar._1, Ident(fieldAndVar._2.name), tag, in))
    })
    Match(Ident(newTermName("number")), cases)
  }

  private def readField(f: Field, readIntoVar: Ident, tag: c.Expr[Int], in: c.Expr[CodedInputStream]): Tree = {
    val parsedValue = parseField(f, tag, in)
    val fieldValue = f match {
      case f: Scalar                        => reify { Some(parsedValue.splice) }
      case f: RepeatedPrimitive if f.packed => reify { parsedValue.splice }
      case f: Repeated                      => reify { c.Expr[Seq[_]](readIntoVar).splice :+ parsedValue.splice }
    }
    Assign(readIntoVar, fieldValue.tree)
  }

  private def checkAllRequiredFieldsAreProvided(varDefs: Map[Field, ValDef]): List[Tree] = varDefs.to[List]
    .flatMap { fieldAndVar => fieldAndVar._1 match {
        case f: Scalar if !f.optional => Some(reify {
          require(c.Expr[Option[_]](Ident(fieldAndVar._2.name)).splice.isDefined, "Field must be set")
        }.tree)
        case _                        => None
      }
    }

  private def constructMessage[T](m: Message, varDefs: Map[Field, ValDef]): c.Expr[T] = {
    val args = varDefs.to[List].sortBy(_._1.number).map { fieldAndVarDef =>
      fieldAndVarDef._1 match {
        case f: Scalar if !f.optional => Select(Ident(fieldAndVarDef._2.name), newTermName("get"))
        case _                        => Ident(fieldAndVarDef._2.name)
      }
    }
    c.Expr(Apply(Select(New(TypeTree(m.actualType)), nme.CONSTRUCTOR), args))
  }
}
