package me.alexbool.macrobuf.macros

import scala.reflect.macros.WhiteboxContext
import me.alexbool.macrobuf.MessageMetadata
import com.google.protobuf.{WireFormat, CodedInputStream}

private[macros] class ParserHelper[C <: WhiteboxContext](val c: C) {

  val mm = MessageMetadata[c.universe.type](c.universe)

  import c.universe._
  import mm._
  import WireFormat._

  private def parsePrimitive(tpe: c.Type, in: c.Expr[CodedInputStream]): c.Expr[Any] = {
    import c.universe.definitions._
    if      (tpe =:= IntTpe)         reify { in.splice.readInt32()         }
    else if (tpe =:= LongTpe)        reify { in.splice.readInt64()         }
    else if (tpe =:= ShortTpe)       reify { in.splice.readInt32().toShort }
    else if (tpe =:= BooleanTpe)     reify { in.splice.readBool()          }
    else if (tpe =:= FloatTpe)       reify { in.splice.readFloat()         }
    else if (tpe =:= DoubleTpe)      reify { in.splice.readDouble()        }
    else if (tpe =:= typeOf[String]) reify { in.splice.readString()        }
    else throw new IllegalArgumentException("Unsupported primitive type")
  }

  private def parsePrimitiveAndRequireWireType(tpe: c.Type, tag: c.Expr[Int], in: c.Expr[CodedInputStream]): c.Expr[Any] = {
    val requireWireTypeExpr = requireWireTypeForPrimitive(tpe, tag)
    val parseExpr = parsePrimitive(tpe, in)
    // XXX Flatten 'require' block of exprs
    reify { requireWireTypeExpr.splice; parseExpr.splice }
  }

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
    case _: Primitive | _: RepeatedPrimitive => parsePrimitiveAndRequireWireType(f.actualType, tag, in)
    case m: MessageField                     => parseDelimited(m, in)
  }

  def parseMessage[T](m: MessageObject, in: c.Expr[CodedInputStream]): c.Expr[T] = {
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
    val tag = c.Expr[Int](Ident(TermName("tag")))
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

  def parseDelimited[T](m: MessageObject, in: c.Expr[CodedInputStream]): c.Expr[T] = {
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
    c.Expr[Seq[Any]](q"""
      ${requireWireFormat(tag, WIRETYPE_LENGTH_DELIMITED)}
      val length = $in.readRawVarint32()
      val oldLimit = $in.pushLimit(length)
      val result = new Iterator[${f.actualType}] {
        def hasNext = !$in.isAtEnd
        def next() = ${parsePrimitive(f.actualType, in)}
      }.to[Seq]
      $in.popLimit(oldLimit)
      result
    """)
  }

  private def declareVars(m: Message): Map[Field, ValDef] = {
    def varDefForField(f: Field): ValDef = {
      val tpt: Tree = f match {
        case _: RequiredField | _: OptionalField => AppliedTypeTree(Ident(TypeName("Option")), List(TypeTree(f.actualType)))
        case _: RepeatedField                    => AppliedTypeTree(Ident(TypeName("Seq")), List(TypeTree(f.actualType)))
      }
      val rhs: Tree = f match {
        case _: RequiredField | _: OptionalField => reify { None  }.tree
        case f: RepeatedField                    => reify { Seq() }.tree
      }
      ValDef(Modifiers(Flag.MUTABLE), TermName(s"${f.fieldName}${f.number}"), tpt, rhs)
    }
    m.fields.map(f => (f, varDefForField(f))).toMap
  }

  private def patternMatchOnFieldNumber(varDefs: Map[Field, ValDef], tag: c.Expr[Int], in: c.Expr[CodedInputStream]): Match = {
    val cases: List[CaseDef] = varDefs.to[List].map(fieldAndVar => {
      CaseDef(Literal(Constant(fieldAndVar._1.number)), EmptyTree, readField(fieldAndVar._1, Ident(fieldAndVar._2.name), tag, in))
    })
    Match(Ident(TermName("number")), cases)
  }

  private def readField(f: Field, readIntoVar: Ident, tag: c.Expr[Int], in: c.Expr[CodedInputStream]): Tree = {
    val parsedValue = parseField(f, tag, in)
    val fieldValue = f match {
      case _: RequiredField | _: OptionalField => reify { Some(parsedValue.splice) }
      case f: RepeatedPrimitive if f.packed    => reify { parsedValue.splice }
      case f: RepeatedField                    => reify { c.Expr[Seq[_]](readIntoVar).splice :+ parsedValue.splice }
    }
    Assign(readIntoVar, fieldValue.tree)
  }

  private def checkAllRequiredFieldsAreProvided(varDefs: Map[Field, ValDef]): List[Tree] = varDefs.to[List]
    .flatMap { fieldAndVar => fieldAndVar._1 match {
        case f: RequiredField => Some(reify {
          require(c.Expr[Option[_]](Ident(fieldAndVar._2.name)).splice.isDefined, "Field must be set")
        }.tree)
        case _ => None
      }
    }

  private def constructMessage[T](m: MessageObject, varDefs: Map[Field, ValDef]): c.Expr[T] = {
    val args = varDefs.to[List].sortBy(_._1.number).map { fieldAndVarDef =>
      fieldAndVarDef._1 match {
        case f: RequiredField => Select(Ident(fieldAndVarDef._2.name), TermName("get"))
        case _                => Ident(fieldAndVarDef._2.name)
      }
    }
    c.Expr(Apply(Select(New(TypeTree(m.actualType)), nme.CONSTRUCTOR), args))
  }
}
