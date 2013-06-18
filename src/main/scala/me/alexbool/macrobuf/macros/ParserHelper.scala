package me.alexbool.macrobuf.macros

import scala.reflect.macros.Context
import me.alexbool.macrobuf.MessageMetadata
import com.google.protobuf.{WireFormat, CodedInputStream}

class ParserHelper[C <: Context](val c: C) {

  import c.universe._
  import WireFormat._

  val mm = MessageMetadata[c.universe.type](c.universe)

  def parsePrimitive(tpe: c.Type)(tag: c.Expr[Int], in: c.Expr[CodedInputStream]): c.Expr[Any] = {
    import c.universe.definitions._
    if      (tpe =:= IntTpe)         reify { requireWireFormat(tag, WIRETYPE_VARINT).splice;           readInt(in).splice     }
    else if (tpe =:= LongTpe)        reify { requireWireFormat(tag, WIRETYPE_VARINT).splice;           readLong(in).splice    }
    else if (tpe =:= ShortTpe)       reify { requireWireFormat(tag, WIRETYPE_VARINT).splice;           readShort(in).splice   }
    else if (tpe =:= BooleanTpe)     reify { requireWireFormat(tag, WIRETYPE_VARINT).splice;           readBoolean(in).splice }
    else if (tpe =:= FloatTpe)       reify { requireWireFormat(tag, WIRETYPE_FIXED32).splice;          readFloat(in).splice   }
    else if (tpe =:= DoubleTpe)      reify { requireWireFormat(tag, WIRETYPE_FIXED64).splice;          readDouble(in).splice  }
    else if (tpe =:= typeOf[String]) reify { requireWireFormat(tag, WIRETYPE_LENGTH_DELIMITED).splice; readString(in).splice  }
    else throw new IllegalArgumentException("Unsupported primitive type")
  }

  // Parsers for primitive types
  def readInt(in: c.Expr[CodedInputStream]): c.Expr[Int]         = reify { in.splice.readInt32() }
  def readLong(in: c.Expr[CodedInputStream]): c.Expr[Long]       = reify { in.splice.readInt64() }
  def readShort(in: c.Expr[CodedInputStream]): c.Expr[Short]     = reify { in.splice.readInt32().asInstanceOf[Short] }
  def readBoolean(in: c.Expr[CodedInputStream]): c.Expr[Boolean] = reify { in.splice.readBool() }
  def readFloat(in: c.Expr[CodedInputStream]): c.Expr[Float]     = reify { in.splice.readFloat() }
  def readDouble(in: c.Expr[CodedInputStream]): c.Expr[Double]   = reify { in.splice.readDouble() }
  def readString(in: c.Expr[CodedInputStream]): c.Expr[String]   = reify { in.splice.readString() }

  def requireWireFormat(tag: c.Expr[Int], wf: Int): c.Expr[Unit] = reify {
    val actualWf = tag.splice & (1 << 3) - 1 // XXX Kinda hack
    require(actualWf == wf, s"Wrong field wire format: found $actualWf, expected $wf")
  }

  def parseField(f: mm.Field, in: c.Expr[CodedInputStream]): c.Expr[Any] = {
    ???
  }

  def parseMessage(m: mm.Message, in: c.Expr[CodedInputStream]): c.Expr[Any] = {
    // 1. Declare vars
    // 2. Loop while !in.isAtEnd(): read tag, decode field number and type
    // 3. Pattern match on number
    // 4. Require wire type matches field metadata, throw exception if needed
    // 5. Read value from loop and assign it to the matching var declared at step 1
    // 6. Go to step 2
    // 7. Check that all required fields are provided
    // 8. Construct message
    // 9. PROFIT!!!
    val varDefs = declareVars(m)
    val varDefsExprs = varDefs.values.map(c.Expr(_))
    ???
  }

  private def declareVars(m: mm.Message): Map[mm.Field, ValDef] = {
    def varDefForField(f: mm.Field): ValDef = {
      val tpt: Tree = TypeTree(f match {
        case f: mm.Scalar   => typeOf[Option[f.actualType.type]]
        case f: mm.Repeated => f.getter.returnType
      })
      val rhs: Tree = f match {
        case f: mm.Scalar   => reify { None  }.tree
        case f: mm.Repeated => reify { Seq() }.tree
      }
      ValDef(Modifiers(Flag.MUTABLE), newTermName(f.fieldName), tpt, rhs)
    }
    m.fields.map(f => (f, varDefForField(f))).toMap
  }

  private def patternMatchOnFieldNumber(varDefs: Map[mm.Field, ValDef], in: c.Expr[CodedInputStream]): Match = {
    val cases: List[CaseDef] = varDefs.to[List].map(fieldAndVar => {
      CaseDef(Literal(Constant(fieldAndVar._1.number)), EmptyTree, readField(fieldAndVar._1, fieldAndVar._2, in))
    })
    Match(Ident(newTermName("number")), cases)
  }

  private def readField(f: mm.Field, readIntoVar: ValDef, in: c.Expr[CodedInputStream]): Tree =
    Assign(readIntoVar, parseField(f, in).tree)
}
